package io.jseval.parser

enum Precendence(code: Int) {
  case MESSAGE extends Precendence(100)
  case UNARY extends Precendence(90) // ! -
  case PRODUCT extends Precendence(80) // * +
  case TERM extends Precendence(70) // + -
  case COMPARISON extends Precendence(50) // < > <= >=
  case EQUALITY extends Precendence(40) // == !=
  case LOGICAL_AND extends Precendence(30) // AND OR
  case LOGICAL_OR extends Precendence(30) // AND OR
  case RECORD extends Precendence(20) // ,
  case ASSIGNMENT extends Precendence(10)
}
