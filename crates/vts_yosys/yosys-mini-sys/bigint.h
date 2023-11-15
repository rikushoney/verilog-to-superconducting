#pragma once

#include <string>

/* A NumberlikeArray<Blk> object holds a heap-allocated array of Blk with a
 * length and a capacity and provides basic memory management features.
 * BigUnsigned and BigUnsignedInABase both subclass it.
 *
 * NumberlikeArray provides no information hiding.  Subclasses should use
 * nonpublic inheritance and manually expose members as desired using
 * declarations like this:
 *
 * public:
 *     NumberlikeArray< the-type-argument >::getLength;
 */
template <class Blk> class NumberlikeArray {
public:
  // Type for the index of a block in the array
  typedef unsigned int Index;
  // The number of bits in a block, defined below.
  static const unsigned int N;

  // The current allocated capacity of this NumberlikeArray (in blocks)
  Index cap;
  // The actual length of the value stored in this NumberlikeArray (in blocks)
  Index len;
  // Heap-allocated array of the blocks (can be NULL if len == 0)
  Blk *blk;

  // Constructs a ``zero'' NumberlikeArray with the given capacity.
  NumberlikeArray(Index c) : cap(c), len(0) {
    blk = (cap > 0) ? (new Blk[cap]) : nullptr;
  }

  /* Constructs a zero NumberlikeArray without allocating a backing array.
   * A subclass that doesn't know the needed capacity at initialization
   * time can use this constructor and then overwrite blk without first
   * deleting it. */
  NumberlikeArray() : cap(0), len(0) { blk = nullptr; }

  // Destructor.  Note that `delete NULL' is a no-op.
  ~NumberlikeArray() { delete[] blk; }

  /* Ensures that the array has at least the requested capacity; may
   * destroy the contents. */
  void allocate(Index c);

  /* Ensures that the array has at least the requested capacity; does not
   * destroy the contents. */
  void allocateAndCopy(Index c);

  // Copy constructor
  NumberlikeArray(const NumberlikeArray<Blk> &x);

  // Assignment operator
  void operator=(const NumberlikeArray<Blk> &x);

  // Constructor that copies from a given array of blocks
  NumberlikeArray(const Blk *b, Index blen);

  // ACCESSORS
  Index getCapacity() const { return cap; }
  Index getLength() const { return len; }
  Blk getBlock(Index i) const { return blk[i]; }
  bool isEmpty() const { return len == 0; }

  /* Equality comparison: checks if both objects have the same length and
   * equal (==) array elements to that length.  Subclasses may wish to
   * override. */
  bool operator==(const NumberlikeArray<Blk> &x) const;

  bool operator!=(const NumberlikeArray<Blk> &x) const {
    return !operator==(x);
  }
};

/* BEGIN TEMPLATE DEFINITIONS.  They are present here so that source files that
 * include this header file can generate the necessary real definitions. */

template <class Blk>
const unsigned int NumberlikeArray<Blk>::N = 8 * sizeof(Blk);

template <class Blk> void NumberlikeArray<Blk>::allocate(Index c) {
  // If the requested capacity is more than the current capacity...
  if (c > cap) {
    // Delete the old number array
    delete[] blk;
    // Allocate the new array
    cap = c;
    blk = new Blk[cap];
  }
}

template <class Blk> void NumberlikeArray<Blk>::allocateAndCopy(Index c) {
  // If the requested capacity is more than the current capacity...
  if (c > cap) {
    Blk *oldBlk = blk;
    // Allocate the new number array
    cap = c;
    blk = new Blk[cap];
    // Copy number blocks
    Index i;
    for (i = 0; i < len; i++)
      blk[i] = oldBlk[i];
    // Delete the old array
    delete[] oldBlk;
  }
}

template <class Blk>
NumberlikeArray<Blk>::NumberlikeArray(const NumberlikeArray<Blk> &x)
    : len(x.len) {
  // Create array
  cap = len;
  blk = new Blk[cap];
  // Copy blocks
  Index i;
  for (i = 0; i < len; i++)
    blk[i] = x.blk[i];
}

template <class Blk>
void NumberlikeArray<Blk>::operator=(const NumberlikeArray<Blk> &x) {
  /* Calls like a = a have no effect; catch them before the aliasing
   * causes a problem */
  if (this == &x)
    return;
  // Copy length
  len = x.len;
  // Expand array if necessary
  allocate(len);
  // Copy number blocks
  Index i;
  for (i = 0; i < len; i++)
    blk[i] = x.blk[i];
}

template <class Blk>
NumberlikeArray<Blk>::NumberlikeArray(const Blk *b, Index blen)
    : cap(blen), len(blen) {
  // Create array
  blk = new Blk[cap];
  // Copy blocks
  Index i;
  for (i = 0; i < len; i++)
    blk[i] = b[i];
}

template <class Blk>
bool NumberlikeArray<Blk>::operator==(const NumberlikeArray<Blk> &x) const {
  if (len != x.len)
    // Definitely unequal.
    return false;
  else {
    // Compare corresponding blocks one by one.
    Index i;
    for (i = 0; i < len; i++)
      if (blk[i] != x.blk[i])
        return false;
    // No blocks differed, so the objects are equal.
    return true;
  }
}

/* A BigUnsigned object represents a nonnegative integer of size limited only by
 * available memory.  BigUnsigneds support most mathematical operators and can
 * be converted to and from most primitive integer types.
 *
 * The number is stored as a NumberlikeArray of unsigned longs as if it were
 * written in base 256^sizeof(unsigned long).  The least significant block is
 * first, and the length is such that the most significant block is nonzero. */
class BigUnsigned : protected NumberlikeArray<unsigned long> {

public:
  // Enumeration for the result of a comparison.
  enum CmpRes { less = -1, equal = 0, greater = 1 };

  // BigUnsigneds are built with a Blk type of unsigned long.
  typedef unsigned long Blk;

  typedef NumberlikeArray<Blk>::Index Index;
  using NumberlikeArray<Blk>::N;

protected:
  // Creates a BigUnsigned with a capacity; for internal use.
  BigUnsigned(int, Index c) : NumberlikeArray<Blk>(0, c) {}

  // Decreases len to eliminate any leading zero blocks.
  void zapLeadingZeros() {
    while (len > 0 && blk[len - 1] == 0)
      len--;
  }

public:
  // Constructs zero.
  BigUnsigned() : NumberlikeArray<Blk>() {}

  // Copy constructor
  BigUnsigned(const BigUnsigned &x) : NumberlikeArray<Blk>(x) {}

  // Assignment operator
  void operator=(const BigUnsigned &x) { NumberlikeArray<Blk>::operator=(x); }

  // Constructor that copies from a given array of blocks.
  BigUnsigned(const Blk *b, Index blen) : NumberlikeArray<Blk>(b, blen) {
    // Eliminate any leading zeros we may have been passed.
    zapLeadingZeros();
  }

  // Destructor.  NumberlikeArray does the delete for us.
  ~BigUnsigned() {}

  // Constructors from primitive integer types
  BigUnsigned(unsigned long x);
  BigUnsigned(long x);
  BigUnsigned(unsigned int x);
  BigUnsigned(int x);
  BigUnsigned(unsigned short x);
  BigUnsigned(short x);

protected:
  // Helpers
  template <class X> void initFromPrimitive(X x);
  template <class X> void initFromSignedPrimitive(X x);

public:
  /* Converters to primitive integer types
   * The implicit conversion operators caused trouble, so these are now
   * named. */
  unsigned long toUnsignedLong() const;
  long toLong() const;
  unsigned int toUnsignedInt() const;
  int toInt() const;
  unsigned short toUnsignedShort() const;
  short toShort() const;

protected:
  // Helpers
  template <class X> X convertToSignedPrimitive() const;
  template <class X> X convertToPrimitive() const;

public:
  // BIT/BLOCK ACCESSORS

  // Expose these from NumberlikeArray directly.
  using NumberlikeArray<Blk>::getCapacity;
  using NumberlikeArray<Blk>::getLength;

  /* Returns the requested block, or 0 if it is beyond the length (as if
   * the number had 0s infinitely to the left). */
  Blk getBlock(Index i) const { return i >= len ? 0 : blk[i]; }
  /* Sets the requested block.  The number grows or shrinks as necessary. */
  void setBlock(Index i, Blk newBlock);

  // The number is zero if and only if the canonical length is zero.
  bool isZero() const { return NumberlikeArray<Blk>::isEmpty(); }

  /* Returns the length of the number in bits, i.e., zero if the number
   * is zero and otherwise one more than the largest value of bi for
   * which getBit(bi) returns true. */
  Index bitLength() const;
  /* Get the state of bit bi, which has value 2^bi.  Bits beyond the
   * number's length are considered to be 0. */
  bool getBit(Index bi) const {
    return (getBlock(bi / N) & (Blk(1) << (bi % N))) != 0;
  }
  /* Sets the state of bit bi to newBit.  The number grows or shrinks as
   * necessary. */
  void setBit(Index bi, bool newBit);

  // COMPARISONS

  // Compares this to x like Perl's <=>
  CmpRes compareTo(const BigUnsigned &x) const;

  // Ordinary comparison operators
  bool operator==(const BigUnsigned &x) const {
    return NumberlikeArray<Blk>::operator==(x);
  }
  bool operator!=(const BigUnsigned &x) const {
    return NumberlikeArray<Blk>::operator!=(x);
  }
  bool operator<(const BigUnsigned &x) const { return compareTo(x) == less; }
  bool operator<=(const BigUnsigned &x) const {
    return compareTo(x) != greater;
  }
  bool operator>=(const BigUnsigned &x) const { return compareTo(x) != less; }
  bool operator>(const BigUnsigned &x) const { return compareTo(x) == greater; }

  /*
   * BigUnsigned and BigInteger both provide three kinds of operators.
   * Here ``big-integer'' refers to BigInteger or BigUnsigned.
   *
   * (1) Overloaded ``return-by-value'' operators:
   *     +, -, *, /, %, unary -, &, |, ^, <<, >>.
   * Big-integer code using these operators looks identical to code using
   * the primitive integer types.  These operators take one or two
   * big-integer inputs and return a big-integer result, which can then
   * be assigned to a BigInteger variable or used in an expression.
   * Example:
   *     BigInteger a(1), b = 1;
   *     BigInteger c = a + b;
   *
   * (2) Overloaded assignment operators:
   *     +=, -=, *=, /=, %=, flipSign, &=, |=, ^=, <<=, >>=, ++, --.
   * Again, these are used on big integers just like on ints.  They take
   * one writable big integer that both provides an operand and receives a
   * result.  Most also take a second read-only operand.
   * Example:
   *     BigInteger a(1), b(1);
   *     a += b;
   *
   * (3) Copy-less operations: `add', `subtract', etc.
   * These named methods take operands as arguments and store the result
   * in the receiver (*this), avoiding unnecessary copies and allocations.
   * `divideWithRemainder' is special: it both takes the dividend from and
   * stores the remainder into the receiver, and it takes a separate
   * object in which to store the quotient.  NOTE: If you are wondering
   * why these don't return a value, you probably mean to use the
   * overloaded return-by-value operators instead.
   *
   * Examples:
   *     BigInteger a(43), b(7), c, d;
   *
   *     c = a + b;   // Now c == 50.
   *     c.add(a, b); // Same effect but without the two copies.
   *
   *     c.divideWithRemainder(b, d);
   *     // 50 / 7; now d == 7 (quotient) and c == 1 (remainder).
   *
   *     // ``Aliased'' calls now do the right thing using a temporary
   *     // copy, but see note on `divideWithRemainder'.
   *     a.add(a, b);
   */

  // COPY-LESS OPERATIONS

  // These 8: Arguments are read-only operands, result is saved in *this.
  void add(const BigUnsigned &a, const BigUnsigned &b);
  void subtract(const BigUnsigned &a, const BigUnsigned &b);
  void multiply(const BigUnsigned &a, const BigUnsigned &b);
  void bitAnd(const BigUnsigned &a, const BigUnsigned &b);
  void bitOr(const BigUnsigned &a, const BigUnsigned &b);
  void bitXor(const BigUnsigned &a, const BigUnsigned &b);
  /* Negative shift amounts translate to opposite-direction shifts,
   * except for -2^(8*sizeof(int)-1) which is unimplemented. */
  void bitShiftLeft(const BigUnsigned &a, int b);
  void bitShiftRight(const BigUnsigned &a, int b);

  /* `a.divideWithRemainder(b, q)' is like `q = a / b, a %= b'.
   * / and % use semantics similar to Knuth's, which differ from the
   * primitive integer semantics under division by zero.  See the
   * implementation in BigUnsigned.cc for details.
   * `a.divideWithRemainder(b, a)' throws an exception: it doesn't make
   * sense to write quotient and remainder into the same variable. */
  void divideWithRemainder(const BigUnsigned &b, BigUnsigned &q);

  /* `divide' and `modulo' are no longer offered.  Use
   * `divideWithRemainder' instead. */

  // OVERLOADED RETURN-BY-VALUE OPERATORS
  BigUnsigned operator+(const BigUnsigned &x) const;
  BigUnsigned operator-(const BigUnsigned &x) const;
  BigUnsigned operator*(const BigUnsigned &x) const;
  BigUnsigned operator/(const BigUnsigned &x) const;
  BigUnsigned operator%(const BigUnsigned &x) const;
  /* OK, maybe unary minus could succeed in one case, but it really
   * shouldn't be used, so it isn't provided. */
  BigUnsigned operator&(const BigUnsigned &x) const;
  BigUnsigned operator|(const BigUnsigned &x) const;
  BigUnsigned operator^(const BigUnsigned &x) const;
  BigUnsigned operator<<(int b) const;
  BigUnsigned operator>>(int b) const;

  // OVERLOADED ASSIGNMENT OPERATORS
  void operator+=(const BigUnsigned &x);
  void operator-=(const BigUnsigned &x);
  void operator*=(const BigUnsigned &x);
  void operator/=(const BigUnsigned &x);
  void operator%=(const BigUnsigned &x);
  void operator&=(const BigUnsigned &x);
  void operator|=(const BigUnsigned &x);
  void operator^=(const BigUnsigned &x);
  void operator<<=(int b);
  void operator>>=(int b);

  /* INCREMENT/DECREMENT OPERATORS
   * To discourage messy coding, these do not return *this, so prefix
   * and postfix behave the same. */
  void operator++();
  void operator++(int);
  void operator--();
  void operator--(int);

  // Helper function that needs access to BigUnsigned internals
  friend Blk getShiftedBlock(const BigUnsigned &num, Index x, unsigned int y);

  // See BigInteger.cc.
  template <class X>
  friend X convertBigUnsignedToPrimitiveAccess(const BigUnsigned &a);
};

/* Implementing the return-by-value and assignment operators in terms of the
 * copy-less operations.  The copy-less operations are responsible for making
 * any necessary temporary copies to work around aliasing. */

inline BigUnsigned BigUnsigned::operator+(const BigUnsigned &x) const {
  BigUnsigned ans;
  ans.add(*this, x);
  return ans;
}
inline BigUnsigned BigUnsigned::operator-(const BigUnsigned &x) const {
  BigUnsigned ans;
  ans.subtract(*this, x);
  return ans;
}
inline BigUnsigned BigUnsigned::operator*(const BigUnsigned &x) const {
  BigUnsigned ans;
  ans.multiply(*this, x);
  return ans;
}
inline BigUnsigned BigUnsigned::operator/(const BigUnsigned &x) const {
  if (x.isZero())
    throw "BigUnsigned::operator /: division by zero";
  BigUnsigned q, r;
  r = *this;
  r.divideWithRemainder(x, q);
  return q;
}
inline BigUnsigned BigUnsigned::operator%(const BigUnsigned &x) const {
  if (x.isZero())
    throw "BigUnsigned::operator %: division by zero";
  BigUnsigned q, r;
  r = *this;
  r.divideWithRemainder(x, q);
  return r;
}
inline BigUnsigned BigUnsigned::operator&(const BigUnsigned &x) const {
  BigUnsigned ans;
  ans.bitAnd(*this, x);
  return ans;
}
inline BigUnsigned BigUnsigned::operator|(const BigUnsigned &x) const {
  BigUnsigned ans;
  ans.bitOr(*this, x);
  return ans;
}
inline BigUnsigned BigUnsigned::operator^(const BigUnsigned &x) const {
  BigUnsigned ans;
  ans.bitXor(*this, x);
  return ans;
}
inline BigUnsigned BigUnsigned::operator<<(int b) const {
  BigUnsigned ans;
  ans.bitShiftLeft(*this, b);
  return ans;
}
inline BigUnsigned BigUnsigned::operator>>(int b) const {
  BigUnsigned ans;
  ans.bitShiftRight(*this, b);
  return ans;
}

inline void BigUnsigned::operator+=(const BigUnsigned &x) { add(*this, x); }
inline void BigUnsigned::operator-=(const BigUnsigned &x) {
  subtract(*this, x);
}
inline void BigUnsigned::operator*=(const BigUnsigned &x) {
  multiply(*this, x);
}
inline void BigUnsigned::operator/=(const BigUnsigned &x) {
  if (x.isZero())
    throw "BigUnsigned::operator /=: division by zero";
  /* The following technique is slightly faster than copying *this first
   * when x is large. */
  BigUnsigned q;
  divideWithRemainder(x, q);
  // *this contains the remainder, but we overwrite it with the quotient.
  *this = q;
}
inline void BigUnsigned::operator%=(const BigUnsigned &x) {
  if (x.isZero())
    throw "BigUnsigned::operator %=: division by zero";
  BigUnsigned q;
  // Mods *this by x.  Don't care about quotient left in q.
  divideWithRemainder(x, q);
}
inline void BigUnsigned::operator&=(const BigUnsigned &x) { bitAnd(*this, x); }
inline void BigUnsigned::operator|=(const BigUnsigned &x) { bitOr(*this, x); }
inline void BigUnsigned::operator^=(const BigUnsigned &x) { bitXor(*this, x); }
inline void BigUnsigned::operator<<=(int b) { bitShiftLeft(*this, b); }
inline void BigUnsigned::operator>>=(int b) { bitShiftRight(*this, b); }

/* Templates for conversions of BigUnsigned to and from primitive integers.
 * BigInteger.cc needs to instantiate convertToPrimitive, and the uses in
 * BigUnsigned.cc didn't do the trick; I think g++ inlined convertToPrimitive
 * instead of generating linkable instantiations.  So for consistency, I put
 * all the templates here. */

// CONSTRUCTION FROM PRIMITIVE INTEGERS

/* Initialize this BigUnsigned from the given primitive integer.  The same
 * pattern works for all primitive integer types, so I put it into a template to
 * reduce code duplication.  (Don't worry: this is protected and we instantiate
 * it only with primitive integer types.)  Type X could be signed, but x is
 * known to be nonnegative. */
template <class X> void BigUnsigned::initFromPrimitive(X x) {
  if (x == 0)
    ; // NumberlikeArray already initialized us to zero.
  else {
    // Create a single block.  blk is NULL; no need to delete it.
    cap = 1;
    blk = new Blk[1];
    len = 1;
    blk[0] = Blk(x);
  }
}

/* Ditto, but first check that x is nonnegative.  I could have put the check in
 * initFromPrimitive and let the compiler optimize it out for unsigned-type
 * instantiations, but I wanted to avoid the warning stupidly issued by g++ for
 * a condition that is constant in *any* instantiation, even if not in all. */
template <class X> void BigUnsigned::initFromSignedPrimitive(X x) {
  if (x < 0)
    throw "BigUnsigned constructor: "
          "Cannot construct a BigUnsigned from a negative number";
  else
    initFromPrimitive(x);
}

// CONVERSION TO PRIMITIVE INTEGERS

/* Template with the same idea as initFromPrimitive.  This might be slightly
 * slower than the previous version with the masks, but it's much shorter and
 * clearer, which is the library's stated goal. */
template <class X> X BigUnsigned::convertToPrimitive() const {
  if (len == 0)
    // The number is zero; return zero.
    return 0;
  else if (len == 1) {
    // The single block might fit in an X.  Try the conversion.
    X x = X(blk[0]);
    // Make sure the result accurately represents the block.
    if (Blk(x) == blk[0])
      // Successful conversion.
      return x;
    // Otherwise fall through.
  }
  throw "BigUnsigned::to<Primitive>: "
        "Value is too big to fit in the requested type";
}

/* Wrap the above in an x >= 0 test to make sure we got a nonnegative result,
 * not a negative one that happened to convert back into the correct nonnegative
 * one.  (E.g., catch incorrect conversion of 2^31 to the long -2^31.)  Again,
 * separated to avoid a g++ warning. */
template <class X> X BigUnsigned::convertToSignedPrimitive() const {
  X x = convertToPrimitive<X>();
  if (x >= 0)
    return x;
  else
    throw "BigUnsigned::to(Primitive): "
          "Value is too big to fit in the requested type";
}

/* A BigInteger object represents a signed integer of size limited only by
 * available memory.  BigUnsigneds support most mathematical operators and can
 * be converted to and from most primitive integer types.
 *
 * A BigInteger is just an aggregate of a BigUnsigned and a sign.  (It is no
 * longer derived from BigUnsigned because that led to harmful implicit
 * conversions.) */
class BigInteger {

public:
  typedef BigUnsigned::Blk Blk;
  typedef BigUnsigned::Index Index;
  typedef BigUnsigned::CmpRes CmpRes;
  static const CmpRes less = BigUnsigned::less, equal = BigUnsigned::equal,
                      greater = BigUnsigned::greater;
  // Enumeration for the sign of a BigInteger.
  enum Sign { negative = -1, zero = 0, positive = 1 };

protected:
  Sign sign;
  BigUnsigned mag;

public:
  // Constructs zero.
  BigInteger() : sign(zero), mag() {}

  // Copy constructor
  BigInteger(const BigInteger &x) : sign(x.sign), mag(x.mag){};

  // Assignment operator
  void operator=(const BigInteger &x);

  // Constructor that copies from a given array of blocks with a sign.
  BigInteger(const Blk *b, Index blen, Sign s);

  // Nonnegative constructor that copies from a given array of blocks.
  BigInteger(const Blk *b, Index blen) : mag(b, blen) {
    sign = mag.isZero() ? zero : positive;
  }

  // Constructor from a BigUnsigned and a sign
  BigInteger(const BigUnsigned &x, Sign s);

  // Nonnegative constructor from a BigUnsigned
  BigInteger(const BigUnsigned &x) : mag(x) {
    sign = mag.isZero() ? zero : positive;
  }

  // Constructors from primitive integer types
  BigInteger(unsigned long x);
  BigInteger(long x);
  BigInteger(unsigned int x);
  BigInteger(int x);
  BigInteger(unsigned short x);
  BigInteger(short x);

  /* Converters to primitive integer types
   * The implicit conversion operators caused trouble, so these are now
   * named. */
  unsigned long toUnsignedLong() const;
  long toLong() const;
  unsigned int toUnsignedInt() const;
  int toInt() const;
  unsigned short toUnsignedShort() const;
  short toShort() const;

protected:
  // Helper
  template <class X> X convertToUnsignedPrimitive() const;
  template <class X, class UX> X convertToSignedPrimitive() const;

public:
  // ACCESSORS
  Sign getSign() const { return sign; }
  /* The client can't do any harm by holding a read-only reference to the
   * magnitude. */
  const BigUnsigned &getMagnitude() const { return mag; }

  // Some accessors that go through to the magnitude
  Index getLength() const { return mag.getLength(); }
  Index getCapacity() const { return mag.getCapacity(); }
  Blk getBlock(Index i) const { return mag.getBlock(i); }
  bool isZero() const { return sign == zero; } // A bit special

  // COMPARISONS

  // Compares this to x like Perl's <=>
  CmpRes compareTo(const BigInteger &x) const;

  // Ordinary comparison operators
  bool operator==(const BigInteger &x) const {
    return sign == x.sign && mag == x.mag;
  }
  bool operator!=(const BigInteger &x) const { return !operator==(x); };
  bool operator<(const BigInteger &x) const { return compareTo(x) == less; }
  bool operator<=(const BigInteger &x) const { return compareTo(x) != greater; }
  bool operator>=(const BigInteger &x) const { return compareTo(x) != less; }
  bool operator>(const BigInteger &x) const { return compareTo(x) == greater; }

  // OPERATORS -- See the discussion in BigUnsigned.hh.
  void add(const BigInteger &a, const BigInteger &b);
  void subtract(const BigInteger &a, const BigInteger &b);
  void multiply(const BigInteger &a, const BigInteger &b);
  /* See the comment on BigUnsigned::divideWithRemainder.  Semantics
   * differ from those of primitive integers when negatives and/or zeros
   * are involved. */
  void divideWithRemainder(const BigInteger &b, BigInteger &q);
  void negate(const BigInteger &a);

  /* Bitwise operators are not provided for BigIntegers.  Use
   * getMagnitude to get the magnitude and operate on that instead. */

  BigInteger operator+(const BigInteger &x) const;
  BigInteger operator-(const BigInteger &x) const;
  BigInteger operator*(const BigInteger &x) const;
  BigInteger operator/(const BigInteger &x) const;
  BigInteger operator%(const BigInteger &x) const;
  BigInteger operator-() const;

  void operator+=(const BigInteger &x);
  void operator-=(const BigInteger &x);
  void operator*=(const BigInteger &x);
  void operator/=(const BigInteger &x);
  void operator%=(const BigInteger &x);
  void flipSign();

  // INCREMENT/DECREMENT OPERATORS
  void operator++();
  void operator++(int);
  void operator--();
  void operator--(int);
};

// NORMAL OPERATORS
/* These create an object to hold the result and invoke
 * the appropriate put-here operation on it, passing
 * this and x.  The new object is then returned. */
inline BigInteger BigInteger::operator+(const BigInteger &x) const {
  BigInteger ans;
  ans.add(*this, x);
  return ans;
}
inline BigInteger BigInteger::operator-(const BigInteger &x) const {
  BigInteger ans;
  ans.subtract(*this, x);
  return ans;
}
inline BigInteger BigInteger::operator*(const BigInteger &x) const {
  BigInteger ans;
  ans.multiply(*this, x);
  return ans;
}
inline BigInteger BigInteger::operator/(const BigInteger &x) const {
  if (x.isZero())
    throw "BigInteger::operator /: division by zero";
  BigInteger q, r;
  r = *this;
  r.divideWithRemainder(x, q);
  return q;
}
inline BigInteger BigInteger::operator%(const BigInteger &x) const {
  if (x.isZero())
    throw "BigInteger::operator %: division by zero";
  BigInteger q, r;
  r = *this;
  r.divideWithRemainder(x, q);
  return r;
}
inline BigInteger BigInteger::operator-() const {
  BigInteger ans;
  ans.negate(*this);
  return ans;
}

/*
 * ASSIGNMENT OPERATORS
 *
 * Now the responsibility for making a temporary copy if necessary
 * belongs to the put-here operations.  See Assignment Operators in
 * BigUnsigned.hh.
 */
inline void BigInteger::operator+=(const BigInteger &x) { add(*this, x); }
inline void BigInteger::operator-=(const BigInteger &x) { subtract(*this, x); }
inline void BigInteger::operator*=(const BigInteger &x) { multiply(*this, x); }
inline void BigInteger::operator/=(const BigInteger &x) {
  if (x.isZero())
    throw "BigInteger::operator /=: division by zero";
  /* The following technique is slightly faster than copying *this first
   * when x is large. */
  BigInteger q;
  divideWithRemainder(x, q);
  // *this contains the remainder, but we overwrite it with the quotient.
  *this = q;
}
inline void BigInteger::operator%=(const BigInteger &x) {
  if (x.isZero())
    throw "BigInteger::operator %=: division by zero";
  BigInteger q;
  // Mods *this by x.  Don't care about quotient left in q.
  divideWithRemainder(x, q);
}
// This one is trivial
inline void BigInteger::flipSign() { sign = Sign(-sign); }

// std::string conversion routines.  Base 10 only.
std::string bigUnsignedToString(const BigUnsigned &x);
std::string bigIntegerToString(const BigInteger &x);
BigUnsigned stringToBigUnsigned(const std::string &s);
BigInteger stringToBigInteger(const std::string &s);

// Creates a BigInteger from data such as `char's; read below for details.
template <class T>
BigInteger dataToBigInteger(const T *data, BigInteger::Index length,
                            BigInteger::Sign sign);

// Outputs x to os, obeying the flags `dec', `hex', `bin', and `showbase'.
std::ostream &operator<<(std::ostream &os, const BigUnsigned &x);

// Outputs x to os, obeying the flags `dec', `hex', `bin', and `showbase'.
// My somewhat arbitrary policy: a negative sign comes before a base indicator
// (like -0xFF).
std::ostream &operator<<(std::ostream &os, const BigInteger &x);

// BEGIN TEMPLATE DEFINITIONS.

/*
 * Converts binary data to a BigInteger.
 * Pass an array `data', its length, and the desired sign.
 *
 * Elements of `data' may be of any type `T' that has the following
 * two properties (this includes almost all integral types):
 *
 * (1) `sizeof(T)' correctly gives the amount of binary data in one
 * value of `T' and is a factor of `sizeof(Blk)'.
 *
 * (2) When a value of `T' is casted to a `Blk', the low bytes of
 * the result contain the desired binary data.
 */
template <class T>
BigInteger dataToBigInteger(const T *data, BigInteger::Index length,
                            BigInteger::Sign sign) {
  // really ceiling(numBytes / sizeof(BigInteger::Blk))
  unsigned int pieceSizeInBits = 8 * sizeof(T);
  unsigned int piecesPerBlock = sizeof(BigInteger::Blk) / sizeof(T);
  unsigned int numBlocks = (length + piecesPerBlock - 1) / piecesPerBlock;

  // Allocate our block array
  BigInteger::Blk *blocks = new BigInteger::Blk[numBlocks];

  BigInteger::Index blockNum, pieceNum, pieceNumHere;

  // Convert
  for (blockNum = 0, pieceNum = 0; blockNum < numBlocks; blockNum++) {
    BigInteger::Blk curBlock = 0;
    for (pieceNumHere = 0; pieceNumHere < piecesPerBlock && pieceNum < length;
         pieceNumHere++, pieceNum++)
      curBlock |=
          (BigInteger::Blk(data[pieceNum]) << (pieceSizeInBits * pieceNumHere));
    blocks[blockNum] = curBlock;
  }

  // Create the BigInteger.
  BigInteger x(blocks, numBlocks, sign);

  delete[] blocks;
  return x;
}

/* Some mathematical algorithms for big integers.
 * This code is new and, as such, experimental. */

// Returns the greatest common divisor of a and b.
BigUnsigned gcd(BigUnsigned a, BigUnsigned b);

/* Extended Euclidean algorithm.
 * Given m and n, finds gcd g and numbers r, s such that r*m + s*n == g. */
void extendedEuclidean(BigInteger m, BigInteger n, BigInteger &g, BigInteger &r,
                       BigInteger &s);

/* Returns the multiplicative inverse of x modulo n, or throws an exception if
 * they have a common factor. */
BigUnsigned modinv(const BigInteger &x, const BigUnsigned &n);

// Returns (base ^ exponent) % modulus.
BigUnsigned modexp(const BigInteger &base, const BigUnsigned &exponent,
                   const BigUnsigned &modulus);
