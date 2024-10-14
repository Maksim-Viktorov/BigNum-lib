#include <fstream>
#include <intrin.h>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include "BigNum.h"

struct BigNum {
  std::vector<uint64_t> number;
  bool negative = false;
  BigNum(std::vector<uint64_t> number_, bool negative_) : number(number_), negative(negative_) {}
  uint64_t Size() {
    return number.size();
  }
  void Extend(uint64_t size) {
    uint64_t val = (negative ? UINT64_MAX : 0);
    if (size > Size()) {
      uint64_t old_size = Size();
      for (uint64_t i = old_size; i < size; ++i) {
        number.push_back(val);
      }
    }
  }

};

std::string ReadNumber(std::string line) {
  std::stringstream ss(line);
  std::string num;
  for (uint64_t i = 0; i < 3; ++i) {
    std::getline(ss, num, ' ');
  }
  return num;
}

BigNum StringToBigNum(std::string str) {
  uint64_t length = str.length();
  uint64_t limit = 1;
  if (str[0] == '-') {
    limit = 2;
  }
  uint64_t size = (length - 1 - limit) / 16 + ((length - 1 - limit) % 16 != 0);
  std::vector<uint64_t> number(size);
  uint64_t j = 0, i = length - 1, k = 0, shift = 0;
  while (i > limit) {
    uint64_t cur_num;
    if (str[i] > '9') {
      cur_num = str[i] - 'A' + 10;
    }
    else {
      cur_num = str[i] - '0';
    }
    number[j] += (cur_num << shift);
    --i;
    shift += 4;
    if (++k > 15) {
      ++j;
      k = 0;
      shift = 0;
    }
  }
  if (limit == 2) {
    unsigned char carry = 1;
    for (uint64_t i = 0; i < size; ++i) {
      carry = _addcarry_u64(carry, ~number[i], 0, &number[i]);
    }
  }
  BigNum bignum(number, limit - 1);
  return bignum;
}

bool BigNumEqual(BigNum lhs, BigNum rhs) {
  if (lhs.negative != rhs.negative) {
    return false;
  }
  uint64_t lsize = std::min(lhs.Size(), rhs.Size());
  for (uint64_t i = 0; i < lsize; ++i) {
    if (lhs.number[i] != rhs.number[i]) return false;
  }
  int64_t insignificant = lhs.negative;
  if (lhs.Size() > rhs.Size()) {
    for (uint64_t i = lsize; i < lhs.Size(); ++i) {
      if (lhs.number[i] != insignificant) return false;
    }
  } else {
    for (uint64_t i = lsize; i < rhs.Size(); ++i) {
      if (rhs.number[i] != insignificant) return false;
    }
  }
  return true;
}

  void testFile(const char* file_name) {
    std::ifstream inputFile(file_name);
    std::string line;
    getline(inputFile, line);
    while (getline(inputFile, line)) {
      if (line == "") continue;
      auto op1 = StringToBigNum(ReadNumber(line));
      getline(inputFile, line);
      auto op2 = StringToBigNum(ReadNumber(line));
      getline(inputFile, line);
      auto res = StringToBigNum(ReadNumber(line));
      std::vector<uint64_t> quotient_num(op1.Size() - op2.Size() + 1);
      BigNum quotient(quotient_num, false);
      Div(quotient.number, op1.number, op2.number);
      std::cout << (BigNumEqual(quotient, res) ? "correct\n" : "incorrect\n");
    }
  }

int main() {
  testFile("divide.vec");
  return 0;
 }

/* Addition
unsigned char carry = Add(op1.number, op2.number);
if (neg_1 + neg_2 == 0) {
  if (carry == 1) {
    op1.Extend(op1.Size() + 1);
    op1.number[op1.Size() - 1] = 1;
  }
}
else if (neg_1 + neg_2 == 1) {
  op1.negative = !carry;
}
else {
  if (carry == 0) {
    op1.Extend(op1.Size() + 1);
    op1.number[op1.Size() - 1] = UINT64_MAX - 1;
  }
}
*/

/* Subtraction
unsigned char borrow = Sub(op1.number, op2.number);
if (neg_1 + neg_2 == 0) {
  if (borrow == 1) {
    op1.negative = true;
  }
}
else if (neg_1 == 1 && neg_2 == 0) {
  if (borrow == 1) {
    op1.Extend(op1.Size() + 1);
    op1.number[op1.Size() - 1] = UINT64_MAX - 1;
  }
}
else if (neg_1 == 0 && neg_2 == 1) {
  if (borrow == 0) {
    op1.Extend(op1.Size() + 1);
    op1.number[op1.Size() - 1] = 1;
  }
}
else {
  op1.negative = borrow;
}
*/

/*  Multiplication
if (op1.Size() != op2.Size()) {
        uint64_t max_size = std::max(op1.Size(), op2.Size());
        op1.Extend(max_size);
        op2.Extend(max_size);
      }
      int neg_1 = op1.negative, neg_2 = op2.negative;
      std::vector<uint64_t> prod_num(op1.Size() * 2);
      BigNum prod(prod_num, false);
      Mul(prod.number, op1.number, op2.number);
*/