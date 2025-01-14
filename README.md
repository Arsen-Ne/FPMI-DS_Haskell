# FPMI-DS_Haskell

This repository contains a set of Haskell solutions for various tasks related to mathematical functions, fixed-point computations, and functors. The tasks are designed to demonstrate the power and versatility of Haskell in solving problems related to algorithms, recursion, and functional programming.

## Contents

The repository is organized into three main files:

1. **[04Tasks.hs](https://github.com/Arsen-Ne/FPMI-DS_Haskell/blob/main/04Tasks.hs)** - A collection of various mathematical tasks:
   - Greatest Common Divisor (GCD)
   - Power computation (Exponentiation)
   - Fibonacci numbers
   - Perfect numbers check
   - Collatz sequence
   - Delannoy numbers
   - Polynomial evaluation
   - List operations (cloning, zipWith, etc.)
   - Systems of numeral representations
   - Grid path enumeration using Delannoy numbers

2. **[07_Fixed_Point_Tasks.hs](https://github.com/Arsen-Ne/FPMI-DS_Haskell/blob/main/07_Fixed_Point_Tasks.hs)** - A demonstration of fixed-point computations:
   - GCD using `fix` (fixed-point combinator)
   - Fibonacci numbers (both list and index-based)
   - Summing list elements using recursion
   - Finding the minimum element in a list
   - Reversing a list

3. **[09_Functor_Tasks.hs](https://github.com/Arsen-Ne/FPMI-DS_Haskell/blob/main/09_Functor_Tasks.hs)** - Functor implementations for various data structures:
   - `Pair`, `Labelled`, `OneOrTwo`, `MyEither`, `MultiTree`, and `Stream`
   - Each data structure demonstrates how to implement the `Functor` instance and how to apply `fmap` for transformations.
   - Includes a set of example transformations to showcase the `Functor` interface in action.

## Setup and Installation

To run the code, you need to have Haskell installed. You can install the Haskell compiler and tools by following these steps:

1. Install Haskell using [GHCup](https://www.haskell.org/downloads/).
2. Clone this repository:
   ```bash
   git clone https://github.com/yourusername/FPMI-DS_Haskell.git
   cd FPMI-DS_Haskell
   ```
3. Compile and run the Haskell files using ghc or runhaskell
   ```bash
   runhaskell 04Tasks.hs
   runhaskell 07_Fixed_Point_Tasks.hs
   runhaskell 09_Functor_Tasks.hs
   ```

## Code Overview
1. 04Tasks.hs
This file contains a series of functions for various mathematical tasks, including:

GCD: Uses recursion to compute the greatest common divisor.
Power: Efficiently computes exponentiation using a recursive method with a time complexity of O(log n).
Fibonacci: Computes Fibonacci numbers both for a specific index and as an infinite stream.
Perfect Numbers: Checks if a number is perfect (sum of its divisors equals the number).
Collatz Sequence: Computes the length of the Collatz sequence for a given number.
Delannoy Numbers: Computes Delannoy numbers, which count paths in a grid.
Polynomial Evaluation: Evaluates polynomials at a specific value.
List Operations: Includes cloning elements, zipping lists, adding digits, and converting between numeral systems.

2. 07_Fixed_Point_Tasks.hs
This file showcases the use of fixed-point combinators with fix. It includes:

A fixed-point GCD function.
A recursive Fibonacci sequence computed using fix.
List operations like summing and finding the minimum element using recursion.
3. 09_Functor_Tasks.hs
This file contains implementations of the Functor type class for different data structures:

Pair: Represents a pair of elements.
Labelled: Represents a value tagged with an identifier.
OneOrTwo: A type that can contain one or two elements.
MyEither: Represents either a left value or a right value.
MultiTree: A tree structure where each node can have multiple children.
Stream: An infinite stream of values.
The file demonstrates how fmap can be used to apply a function to the values contained in each structure.

## Example Output
### [04Tasks.hs](https://github.com/Arsen-Ne/FPMI-DS_Haskell/blob/main/04Tasks.hs)
```bash
GCD examples (48 18) and (54 24):
6
6

Power examples (2 3) and (3 4):
8
81

Perfect numbers check 6, 28 and 12:
True
True
False

Collatz sequence lengths 13 and 19:
10
21

Delannoy numbers (2 2) and (2 2): 
13
63

Evaluate polynomial ([1, -3, 2] 3) and ([1, 0, 0, -4] 2):
2
4

Clone list elements 3 [1, 2, 3] :
[1,1,1,2,2,2,3,3,3]

ZipWith example (+) [1, 2, 3] [4, 5, 6]:
[5,7,9]

Fibonacci example (10):
[0,1,1,2,3,5,8,13,21,34]
[0,1,1,2,3,5,8,13,21,34]
[7, 3, 10, 0]
[7,3,10,0,20,33,63,116,232,444]
[3, 8, 0]
[3,8,0,11,19,30,60,109,199,368]

From digits (10 [1, 2, 3]) and (2 [1, 0, 1]):
123
5

To digits (10 123) and (2 5 ):
[1,2,3]
[1,0,1]

Digitwise addition:(10 [1, 2, 3] [4, 5, 6]) and (2 [1, 0, 1] [1, 1, 1])
[9,7,5]
[1,1]

Delannoy paths 2 2:
[[0,0,2,2],[0,1,2],[0,2,0,2],[0,2,1],[0,2,2,0],[1,0,2],[1,1],[1,2,0],[2,0,0,2],[2,0,1],[2,0,2,0],[2,1,0],[2,2,0,0]]
```

### [07_Fixed_Point_Tasks.hs](https://github.com/Arsen-Ne/FPMI-DS_Haskell/blob/main/07_Fixed_Point_Tasks.hs)
```bash
Наибольший общий делитель 56 b 98:
14

Бесконечный список чисел Фибоначчи(10)
[0,1,1,2,3,5,8,13,21,34]

 Число Фибоначчи по индексу(10)
55

 Сумма элементов списка [1, 2, 3, 4, 5]
15

 Наименьший элемент в списке [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
1

Реверс списка [1, 2, 3, 4, 5]
[5,4,3,2,1]

```

### [09_Functor_Tasks.hs](https://github.com/Arsen-Ne/FPMI-DS_Haskell/blob/main/09_Functor_Tasks.hs)
```bash
Pair 4 6

Labelled "Warning" "Hello!"

One 10

Two 6 14

MyLeft "Error"

MyRight 20

Node 2 [Node 4 [Leaf],Node 6 [Leaf]]

[2,3,4,5,6]
```




