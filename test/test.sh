# !/bin/bash

assert() {
  # para1 is 'expected', para2 is 'input'
  expected="$1"
  input="$2"
  temp=${input%.*}

  # shell codes
  echo "$input" | ./bin/mycc $input > $temp.s || exit
  gcc -static -o $temp $temp.s
  $temp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert   8  './test/test1.txt'
assert  14  './test/test2.txt'
assert 192  './test/test3.txt'
assert 144  './test/test4.txt'
assert 144  './test/test5.txt'

echo OK
