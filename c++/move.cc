#include <string.h>
#include <string>
#include <iostream>
#include <set>
#include <utility>
#include <vector>

//#include "move.h"

class Internal {
public:
  Internal(const char *s) {
    int n = strlen(s);
    str = new char[n+1];
    std::cout << "New internal " << std::hex << long(str) << std::endl;
    for (int i=0; i<n; i++) {
      str[i] = s[i];
    }
    str[n] = 0;
  }
  ~Internal() {
    std::cout << "Delete internal " << std::hex << long(str) << std::endl;
    delete[] str;
  }
  char *str;
};

class X {
public:
  X():s_("empty") {
    std::cout << "X Constructor empty" << std::endl;
  }
  X(const char *s):s_(s) {
      std::cout << "X Constructor " << s << std::endl;
  }
  X(const X& x):s_(x.s_.str) {
    std::cout << "X copy constructor" << std::endl;
  }
  X& operator=(const X& x) {
    s_ = x.s_;
    std::cout << "X operator = " << s_.str << std::endl;
    return *this;
  }
  ~X() {
    std::cout << "X Destructor " << s_.str << std::endl;
  }
  Internal s_;
};

void foo() {
  X x("qian");
  std::cout << "in foo function " << x.s_.str << std::endl;
  //return x;
}

int main() {
  foo();
  std::cout << "before exit" << std::endl;
}
