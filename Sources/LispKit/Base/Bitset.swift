//
//  Bitset.swift
//  LispKit
//  
//  This is a slightly modified version of the code at https://github.com/lemire/SwiftBitset
//  Copyright © 2019 Daniel Lemire. All rights reserved.
//  
//  Licensed under the Apache License, Version 2.0 (the "License");
//  you may not use this file except in compliance with the License.
//  You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS,
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  See the License for the specific language governing permissions and
//  limitations under the License.
//

import Foundation

///
/// Efficient set container for non-negative integers.
///
public final class Bitset: Sequence, Equatable, CustomStringConvertible,
                           Hashable, ExpressibleByArrayLiteral {
  static let wordSize = 8
  
  /// How many words have been allocated?
  var capacity: Int
  
  /// How many words are used?
  var wordcount: Int
  
  /// Biset storage
  var data: UnsafeMutablePointer<UInt64>
  
  /// Initializer for empty bitset
  public init(capacity: Int = 1) {
    self.capacity = capacity
    self.wordcount = 0
    self.data = UnsafeMutablePointer<UInt64>.allocate(capacity: capacity)
  }
  
  /// Copy initializer
  public init(_ other: Bitset) {
    self.capacity = other.wordcount
    self.wordcount = other.wordcount
    self.data = UnsafeMutablePointer<UInt64>.allocate(capacity: other.wordcount)
    for i in 0..<self.capacity {
      self.data[i] = other.data[i]
    }
  }
  
  deinit {
    data.deallocate()
  }

  /// Make a bitset containing the list of integers, all values must be non-negative
  /// adding the value i to the bitset will cause the use of least (i+8)/8 bytes
  public init(_ allints: [Int]) {
    var mymax = 0
    for i in allints {
      mymax = mymax < i ? i : mymax
    }
    self.wordcount = (mymax+63)/64 + 1
    self.capacity = self.wordcount
    self.data = UnsafeMutablePointer<UInt64>.allocate(capacity: self.wordcount)
    for k in 0..<wordcount {
      self.data[k] = 0
    }
    self.addMany(allints)
  }

  /// Initializing from array literal
  required public init(arrayLiteral elements: Int...) {
    var mymax = 0
    for i in elements {
      mymax = mymax < i ? i : mymax
    }
    wordcount = (mymax+63)/64 + 1
    capacity = wordcount
    data = UnsafeMutablePointer<UInt64>.allocate(capacity:wordcount)
    for k in 0..<wordcount {
      data[k] = 0
    }
    for i in elements {
      add(i)
    }
  }

  // load an uncompressed bitmap from a byte buffer, in ascending order
  // The expected format is equivalent to that of an array of 64-bit unsigned integers stored 
  // using the little endian encoding, except that zero bytes at the end are omitted.
  // This function is compatible with the toData() function.
  public init(bytes: Data) {
    assert(Bitset.wordSize == 8) // this logic is expecting a 64-bit internal representation
    let byteCount = bytes.count
    if (byteCount == 0) {
      self.capacity = 1
      self.wordcount = 0
      self.data = UnsafeMutablePointer<UInt64>.allocate(capacity:capacity)
      return
    }
    self.capacity = (byteCount - 1) / Bitset.wordSize + 1
    self.wordcount = capacity
    self.data = UnsafeMutablePointer<UInt64>.allocate(capacity: self.capacity)
    func iterate<T>(_ pointer: T, _ f: (T, Int, Int) -> UInt64) -> Int {
      var remaining = byteCount
      var offset = 0
      for w in 0..<capacity {
        if remaining < Bitset.wordSize { break }
        // copy entire word - assumes data is aligned to word boundary
        let next = offset + Bitset.wordSize
        var word: UInt64 = f(pointer, offset, w)
        word = CFSwapInt64LittleToHost(word)
        remaining -= Bitset.wordSize
        offset = next
        data[w] = word
      }
      return remaining
    }
    var remaining = byteCount
    if remaining > Bitset.wordSize {
      remaining = bytes.withUnsafeBytes { (pointer: UnsafeRawBufferPointer) -> Int in
        iterate(pointer) { (pointer, offset, _) in
          pointer.load(fromByteOffset: offset, as: UInt64.self)
        }
      }
    }
    if remaining > 0 {
      // copy last word fragment
      // manual byte copy is about 50% faster than `copyBytes` with `withUnsafeMutableBytes`
      var word: UInt64 = 0
      let offset = byteCount - remaining
      for b in 0..<remaining {
        let byte = UInt64(clamping: bytes[offset + b])
        word = word | (byte << (b * 8))
      }
      data[capacity-1] = word
    }
    // TODO: shrink bitmap according to MSB
  }

  // store as uncompressed bitmap as a byte buffer in ascending order, with a bytes
  // size that captures the most significant bit, or an empty instance if no bits are
  // present. The format is equivalent to that of an array of 64-bit unsigned integers
  // stored using the little endian encoding, except that zero bytes at the end are
  // omitted. This function is compatible with the init(bytes: Data) constructor. 
  public func toData() -> Data {
    assert(Bitset.wordSize == 8) // this logic is expecting a 64-bit internal representation
    let heighestWord = self.heighestWord()
    if heighestWord < 0 { return Data() }
    let lastWord = Int64(bitPattern: data[heighestWord])
    let lastBit = Int(flsll(lastWord))
    let lastBytes = lastBit == 0 ? 0 : (lastBit - 1) / 8 + 1
    let size = heighestWord * Bitset.wordSize + lastBytes
    var output = Data(capacity: size)
    for w in 0...heighestWord {
      var word = CFSwapInt64HostToLittle(data[w])
      let byteCount = w == heighestWord ? lastBytes : Bitset.wordSize
      let bytes = Data(bytes: &word, count: byteCount) // about 10x faster than memory copy
      output.append(bytes)
    }
    return output
  }

  public typealias Element = Int

  // return an empty bitset
  public static var allZeros: Bitset {
    return Bitset()
  }

  // union between two bitsets, producing a new bitset
  public static func | (lhs: Bitset, rhs: Bitset) -> Bitset {
    let mycopy = Bitset(lhs)
    mycopy.union(rhs)
    return mycopy
  }

  // compute the union between two bitsets inplace
  public static func |= (lhs: Bitset, rhs: Bitset) {
    lhs.union(rhs)
  }


  // difference between two bitsets, producing a new bitset
  public static func - (lhs: Bitset, rhs: Bitset) -> Bitset {
    let mycopy = Bitset(lhs)
    mycopy.difference(rhs)
    return mycopy
  }

  // inplace difference between two bitsets
  public static func -= (lhs: Bitset, rhs: Bitset) {
    lhs.difference(rhs)
  }

  // symmetric difference between two bitsets, producing a new bitset
  public static func ^ (lhs: Bitset, rhs: Bitset) -> Bitset {
    let mycopy = Bitset(lhs)
    mycopy.symmetricDifference(rhs)
    return mycopy
  }

  // inplace symmetric difference between two bitsets
  public static func ^= (lhs: Bitset, rhs: Bitset) {
    lhs.symmetricDifference(rhs)
  }

  // compute the union between two bitsets inplace
  public static func &= (lhs: Bitset, rhs: Bitset) {
    lhs.intersection(rhs)
  }

  // computes the intersection between two bitsets and return a new bitset
  public static func & (lhs: Bitset, rhs: Bitset) -> Bitset {
    let mycopy = Bitset(lhs)
    mycopy.intersection(rhs)
    return mycopy
  }

  // hash value for the bitset
  public func hash(into hasher: inout Hasher) {
    let b: UInt64 = 31
    var hash: UInt64 = 0
    for i in 0..<wordcount {
      let w = data[i]
      hash = hash &* b &+ w
    }
    hash = hash ^ (hash >> 33)
    hash = hash &* 0xff51afd7ed558ccd
    hash = hash ^ (hash >> 33)
    hash = hash &* 0xc4ceb9fe1a85ec53
    hasher.combine(hash)
  }
  
  // returns a string representation of the bitset
  public var description: String {
    var ret = prefix(100).map { $0.description }.joined(separator: ", ")
    if count() >= 100 {
        ret.append(", ...")
    }
    return "{\(ret)}"
  }

  // create an iterator over the values contained in the bitset
  public func makeIterator() -> BitsetIterator {
    return BitsetIterator(self)
  }

  // count how many values have been stored in the bitset (this function is not
  // free of computation)
  public func count() -> Int {
    var sum: Int = 0
    for i in 0..<wordcount {
      let w = data[i]
      sum = sum &+ w.nonzeroBitCount
    }
    return sum
  }
  
  // proxy for "count"
  public func cardinality() -> Int { return count() }

  // add a value to the bitset, all values must be non-negative
  // adding the value i to the bitset will cause the use of least (i+8)/8 bytes
  public func add(_ value: Int) {
    let index = value >> 6
    if index >= self.wordcount { increaseWordCount( index + 1) }
    data[index] |= 1 << (UInt64(value & 63))
  }

  // add all the values  to the bitset
  // adding the value i to the bitset will cause the use of least (i+8)/8 bytes
  public func addMany(_ allints: Int...) {
    var mymax = 0
    for i in allints {
      mymax = mymax < i ? i : mymax
    }
    let maxindex = mymax >> 6
    if maxindex >= self.wordcount {
      increaseWordCount(maxindex + 1)
    }
    for i in allints {
      add(i)
    }
  }

  // add all the values  to the bitset
  // adding the value i to the bitset will cause the use of least (i+8)/8 bytes
  public func addMany(_ allints: [Int]) {
    var mymax = 0
    for i in allints {
      mymax = mymax < i ? i : mymax
    }
    let maxindex = mymax >> 6
    if maxindex >= self.wordcount {
      increaseWordCount(maxindex + 1)
    }
    for i in allints {
      add(i)
    }
  }

  // check that a value is in the bitset, all values must be non-negative
  public func contains(_ value: Int) -> Bool {
    let index = value >> 6
    if index >= self.wordcount {
      return false
    }
    return data[index] & (1 << (UInt64(value & 63))) != 0
  }

  public subscript(value: Int) -> Bool {
    get {
      return contains(value)
    }
    set(newValue) {
      if newValue {
        add(value)
      } else {
        remove(value)
      }
    }
  }

  // compute the intersection (in place) with another bitset
  public func intersection(_ other: Bitset) {
    let mincount = Swift.min(self.wordcount, other.wordcount)
    for i in 0..<mincount { data[i] &= other.data[i] }
    for i in mincount..<self.wordcount { data[i] = 0 }
  }

  // compute the size of the intersection with another bitset
  public func intersectionCount(_ other: Bitset) -> Int {
    let mincount = Swift.min(self.wordcount, other.wordcount)
    var sum = 0
    for i in 0..<mincount { sum = sum &+ ( data[i] & other.data[i]).nonzeroBitCount }
    return sum
  }

  // compute the union (in place) with another bitset
  public func union(_ other: Bitset) {
    let mincount = Swift.min(self.wordcount, other.wordcount)
    for  i in 0..<mincount {
      data[i] |= other.data[i]
    }
    if other.wordcount > self.wordcount {
      self.matchWordCapacity(other.wordcount)
      self.wordcount = other.wordcount
      for i in mincount..<other.wordcount {
        data[i] = other.data[i]
      }
    }
  }

  // compute the size of the union with another bitset
  public func unionCount(_ other: Bitset) -> Int {
    let mincount = Swift.min(self.wordcount, other.wordcount)
    var sum = 0
    for  i in 0..<mincount {
      sum = sum &+ (data[i] | other.data[i]).nonzeroBitCount
    }
    if other.wordcount > self.wordcount {
      for i in mincount..<other.wordcount {
        sum = sum &+ (other.data[i]).nonzeroBitCount
      }
    } else {
      for i in mincount..<self.wordcount {
        sum = sum &+ (data[i]).nonzeroBitCount
      }
    }
    return sum
  }

  // compute the symmetric difference (in place) with another bitset
  public func symmetricDifference(_ other: Bitset) {
    let mincount = Swift.min(self.wordcount, other.wordcount)
    for  i in 0..<mincount {
      data[i] ^= other.data[i]
    }
    if other.wordcount > self.wordcount {
      self.matchWordCapacity(other.wordcount)
      self.wordcount = other.wordcount
      for i in mincount..<other.wordcount {
        data[i] = other.data[i]
      }
    }
  }

  // compute the size union  with another bitset
  public func symmetricDifferenceCount(_ other: Bitset) -> Int {
    let mincount = Swift.min(self.wordcount, other.wordcount)
    var sum = 0
    for  i in 0..<mincount {
      sum = sum &+ (data[i] ^ other.data[i]).nonzeroBitCount
    }
    if other.wordcount > self.wordcount {
      for i in mincount..<other.wordcount {
        sum = sum &+ other.data[i].nonzeroBitCount
      }
    } else {
      for i in mincount..<self.wordcount {
        sum = sum &+ (data[i]).nonzeroBitCount
      }
    }
    return sum
  }

  // compute the difference (in place) with another bitset
  public func difference(_ other: Bitset) {
    let mincount = Swift.min(self.wordcount, other.wordcount)
    for  i in 0..<mincount {
      data[i] &= ~other.data[i]
    }
  }

  // compute the size of the difference with another bitset
  public func differenceCount(_ other: Bitset) -> Int {
    let mincount = Swift.min(self.wordcount, other.wordcount)
    var sum = 0
    for  i in 0..<mincount {
      sum = sum &+ ( data[i] & ~other.data[i]).nonzeroBitCount
    }
    for i in mincount..<self.wordcount {
      sum = sum &+ (data[i]).nonzeroBitCount
    }
    return sum
  }

  // remove a value, must be non-negative
  public func remove(_ value: Int) {
    let index = value >> 6
    if index < self.wordcount {
      data[index] &= ~(1 << UInt64(value & 63))
    }
  }

  // remove a value, if it is present it is removed, otherwise it is added, must
  // be non-negative
  public func flip(_ value: Int) {
    let index = value >> 6
    if index < self.wordcount {
      data[index] ^= 1 << UInt64(value & 63)
    } else {
      increaseWordCount(index + 1)
      data[index] |= 1 << UInt64(value & 63)
    }
  }

  // remove many values, all must be non-negative
  public func removeMany(_ allints: Int...) {
    for i in allints {
      remove(i)
    }
  }

  // return the memory usage of the backing array in bytes
  public func memoryUsage() -> Int {
    return self.capacity * 8
  }

  // check whether the value is empty
  public func isEmpty() -> Bool {
    for i in 0..<wordcount {
      let w = data[i]
      if w != 0 {
        return false
      }
    }
    return true
  }

  // remove all elements, optionally keeping the capacity intact
  public func removeAll(keepingCapacity keepCapacity: Bool = false) {
    wordcount = 0
    if !keepCapacity {
      data.deallocate()
      capacity = 8 // reset to some default
      data = UnsafeMutablePointer<UInt64>.allocate(capacity:capacity)
    }
  }
  
  // Returns the next element in the bitset, starting the search from `current`
  public func next(current: Int = 0) -> Int? {
    var value = current
    var x = value >> 6
    if x >= self.wordcount {
      return nil
    }
    var w = self.data[x]
    w >>= UInt64(value & 63)
    if w != 0 {
      value = value &+ w.trailingZeroBitCount
      return value
    }
    x = x &+ 1
    while x < self.wordcount {
      let w = self.data[x]
      if w != 0 {
        value = x &* 64 &+ w.trailingZeroBitCount
        return value
      }
      x = x &+ 1
    }
    return nil
  }
  
  private static func nextCapacity(mincap: Int) -> Int {
    return 2 * mincap
  }

   // caller is responsible to ensure that index < wordcount
  func increaseWordCount(_ newWordCount: Int) {
    if newWordCount > capacity {
      growWordCapacity(Bitset.nextCapacity(mincap : newWordCount))
    }
    if newWordCount > wordcount {
      for i in wordcount..<newWordCount {
        data[i] = 0
      }
    }
    wordcount = newWordCount
  }

  func growWordCapacity(_ newcapacity: Int) {
    let newdata = UnsafeMutablePointer<UInt64>.allocate(capacity:newcapacity)
    for i in 0..<self.wordcount {
      newdata[i] = self.data[i]
    }
    data.deallocate()
    data = newdata
    self.capacity = newcapacity
  }

  func matchWordCapacity(_ newcapacity: Int) {
    if newcapacity > self.capacity {
      growWordCapacity(newcapacity)
    }
  }

  func heighestWord() -> Int {
    for i in (0..<wordcount).reversed() {
      let w = data[i]
      if w.nonzeroBitCount > 0 { return i }
    }
    return -1
  }

  // checks whether the two bitsets have the same content
  public static func == (lhs: Bitset, rhs: Bitset) -> Bool {
    if lhs.wordcount > rhs.wordcount {
      for  i in rhs.wordcount..<lhs.wordcount  where lhs.data[i] != 0 {
        return false
      }
    } else if lhs.wordcount < rhs.wordcount {
      for i in lhs.wordcount..<rhs.wordcount where  rhs.data[i] != 0 {
        return false
      }
    }
    let mincount = Swift.min(rhs.wordcount, lhs.wordcount)
    for  i in 0..<mincount where rhs.data[i] != lhs.data[i] {
      return false
    }
    return true
  }
}

public struct BitsetIterator: IteratorProtocol {
   let bitset: Bitset
   var value: Int = -1

   init(_ bitset: Bitset) {
       self.bitset = bitset
   }

   public mutating func next() -> Int? {
     value = value &+ 1
     var x = value >> 6
     if x >= bitset.wordcount {
       return nil
     }
     var w = bitset.data[x]
     w >>= UInt64(value & 63)
     if w != 0 {
       value = value &+ w.trailingZeroBitCount
       return value
     }
     x = x &+ 1
     while x < bitset.wordcount {
       let w = bitset.data[x]
       if w != 0 {
         value = x &* 64 &+ w.trailingZeroBitCount
         return value
       }
       x = x &+ 1
     }
     return nil
   }
}
