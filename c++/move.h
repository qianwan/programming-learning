#ifndef MOVE_H
#define MOVE_H

#include <iostream>

template<typename T>
class set {
 public:
  set() {}
  void insert(const T& v) {
    std::cout << "set &" << std::endl;
  }
  void insert(T&& v) {
    std::cout << "set &&" << std::endl;
  }
};

#endif
