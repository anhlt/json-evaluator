package io.jseval.parser

enum Precendence(val code: Int) {
  case MESSAGE extends Precendence(100)
  case UNARY extends Precendence(90) // ! -
  case PRODUCT extends Precendence(80) // * /
  case TERM extends Precendence(70) // + -
  case COMPARISON extends Precendence(50) // < > <= >=
  case EQUALITY extends Precendence(40) // == !=
  case LOGICAL_AND extends Precendence(30) // AND
  case LOGICAL_OR extends Precendence(25) // OR
  case RECORD extends Precendence(20) // ,
  case ARROW extends Precendence(15) // -> 
  case ASSIGNMENT extends Precendence(10) // =
  case LOWEST extends Precendence(0)
}
