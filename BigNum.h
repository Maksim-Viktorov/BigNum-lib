#include <intrin.h>
#include <iostream>

template <typename T> unsigned char Add(T& first, T& second) {
  MakeTheSameSize(first, second);
  unsigned char carry = 0;
  for (uint64_t i = 0; i < first.size(); ++i) {
    carry = _addcarry_u64(carry, first[i], second[i], &first[i]);
  }
  return carry;
}

template <typename T> unsigned char Sub(T& first, T& second) {
  MakeTheSameSize(first, second);
  unsigned char borrow = 0;
  for (int64_t i = 0; i < first.size(); ++i) {
    borrow = _subborrow_u64(borrow, first[i], second[i], &first[i]);
  }
  return borrow;
}

template <typename T> void Mul(T& result, T& first, T& second) {
  MakeTheSameSize(first, second);
  unsigned __int64 size = first.size();
  for (int64_t j = 0; j < size; ++j) {
    if (first[j] == 0) {
      result[size + j] = 0;
      continue;
    }
    unsigned __int64 carry = 0;
    for (int64_t i = 0; i < size; ++i) {
      unsigned __int64 tmp_mul = 0, tmp_carry = 0;
      tmp_mul = _umul128(first[j], second[i], &tmp_carry);
      tmp_carry += _addcarry_u64(0, tmp_mul, carry, &tmp_mul);
      carry = tmp_carry + _addcarry_u64(0, result[i + j], tmp_mul, &result[i + j]);
    }
    result[j + size] = carry;
  }
}

template <typename T> void Div(T& result, T& first, T& second) {
  MakeTheSameSize(first, second);
  unsigned __int64 leading_zeros = __lzcnt64(second[second.size() - 1]);
  unsigned __int64 high = 0;
  if (leading_zeros != 0) {
    unsigned __int64 mask = UINT64_MAX;
    mask <<= (64 - leading_zeros);
    high = first[first.size() - 1] & mask;
    high >>= (64 - leading_zeros);
    for (int i = first.size() - 1; i >= 0; --i) {
      first[i] <<= leading_zeros;
      if (i > 0) {
        unsigned __int64 outlier = first[i - 1] & mask;
        outlier >>= (64 - leading_zeros);
        first[i] |= outlier;
      }
    }
    for (int i = second.size() - 1; i >= 0; --i) {
      second[i] <<= leading_zeros;
      if (i > 0) {
        unsigned __int64 outlier = second[i - 1] & mask;
        outlier >>= 64 - leading_zeros;
        second[i] |= outlier;
      }
    }
  }
  for (int j = first.size() - second.size(); j >= 0; --j) {
    unsigned __int64 quotient = 0, remainder = 0;
    quotient = _udiv128((j == first.size() - second.size() ? high : first[j + second.size()]),
      first[j + second.size() - 1], second[second.size() - 1], &remainder);
    if (second.size() > 1) {
      uint64_t prod_128 = 0;
      uint64_t prod_64 = _umul128(quotient, second[second.size() - 2], &prod_128);
      unsigned char borrow = 0;
      borrow = _subborrow_u64(borrow, first[j + second.size() - 2], prod_64, &prod_64);
      borrow = _subborrow_u64(borrow, remainder, prod_128, &prod_128);
      if (borrow) {
        --quotient;
        unsigned char carry = 0;
        carry = _addcarry_u64(0, remainder, second[second.size() - 1], &remainder);
        prod_64 = _umul128(quotient, second[second.size() - 2], &prod_128);
        borrow = 0;
        borrow = _subborrow_u64(borrow, first[j + second.size() - 2], prod_64, &prod_64);
        borrow = _subborrow_u64(borrow, remainder, prod_128, &prod_128);
        if (!carry && borrow) {
          --quotient;
          remainder += second[second.size() - 1];
        }
      }
    }
    unsigned char borrow = 0;
    uint64_t prod = 0, carry = 0;
    for (uint64_t i = 0; i < second.size(); ++i) {
      uint64_t prev_carry = carry;
      prod = _umul128(quotient, second[i], &carry);
      carry += _addcarry_u64(0, prod, prev_carry, &prod);
      borrow = _subborrow_u64(borrow, first[i + j], prod, &first[i + j]);
    }
    borrow = _subborrow_u64(borrow, (j == first.size() - second.size() ? high : first[j + second.size()]),
                            carry, (j == first.size() - second.size() ? &high : &first[j + second.size()]));
    result[j] = quotient;
    if (borrow) {
      --result[j];
      carry = 0;
      for (int i = 0; i < second.size(); ++i) {
        carry = _addcarry_u64(carry, first[j + i], second[i], &first[j + i]);
      }
      (j == first.size() - second.size() ? ++high : ++first[j + second.size()]);
    }
  }
  if (leading_zeros != 0) {
    unsigned __int64 mask = UINT64_MAX;
    mask >>= (64 - leading_zeros);
    for (int i = 0; i < first.size(); ++i) {
      if (i > 0) {
        unsigned __int64 outlier = first[i] & mask;
        outlier <<= (64 - leading_zeros);
        first[i - 1] |= outlier;
      }
      first[i] >>= leading_zeros;
    }
  }
}

template <typename T> void MakeTheSameSize(T& first, T& second, bool Unsigned = false) {
  T& _short, _long;
  if (first.size() > second.size()) {
    _short = first;
    _long = second;
  } else if (second.size() > first.size()) {
    _short = second;
    _long = first;
  } else {
    return;
  }
  bool leading_zeros = __lzcnt64(_short[_short.size() - 1]);
  int64_t value;
  if (!leading_zeros && !Unsigned) {
    value = INT64_MAX;
  } else {
    value = 0;
  }
  T extended = T<int64_t>(_long.size());
  for (int i = 0; i < _short.size(); ++i) {
    extended[i] = _short[i];
  }
  for (int i = _short.size(); i < _long.size(); ++i) {
    extended[i] = value;
  }
  _short = extended;
}

template <typename T> void Copy(T& first, const T& second) {
  first = T<int64_t>(second.size());
  for (int i = 0; i < second.size(); ++i) {
    first[i] = second[i];
  }
}

template <typename T> bool Cmp(T& first, T& second, bool Unsigned = false) {
  T firstCopy;
  Copy(firstCopy, first);
  return Sub(firstCopy, second);
}

template <typename T> void MontRed(T m, T mInverted, T t) {
  T a;
  Copy(a, t);
  for (int i = 0; i < m.size(); ++i) {
    T u;
    Mul(u, a[i], mInverted);
    u >> b;
    Mul(u, u, m);
    u << b ^ i;
    Add(a, u);
  }
  a >> (b ^ m.size());
  if (!Cmp(a, m)) {
    Sub(a, m);
  }
  return a;
}


template <typename T> void Pow(T number, int power, T module) {

}


