# TamVan19 implementation

This tool is an implementation of the paper [Enumeration of the nondominated set of multiobjective discrete optimization problems, by S. Tamby and D. Vanderpooten](https://pubsonline.informs.org/doi/abs/10.1287/ijoc.2020.0953) written in Haskell.

## Disclaimer



Note that this code has **not** been used to perform the computational experiments of the paper. The proper version is available at the URL provided in the paper, which must have been linked with an older IBM Cplex (c) version. This code has been updated to be used with **IBM Cplex(c) 22.10**.

As you can see in the source code, I am not an expert developper. Despite the reliability of Haskell, this code is provided without any waranty so use it at your own risks. Please do not hesitate to submit any bug.

This code has been tested on linux (*NixOS 22.05*), but should work on other distributions. Please report any problem, especially on *Windows* or *MacOS*.

## Installation

Install [the haskell tool stack](https://docs.haskellstack.org/) a reproducible building environment in haskell. Note that this tool is packaged in most linux distribution and can be installed using the standard package manager.

Clone this repository

```
git clone https://github.com/tambysatya/TamVan19.git
cd TamVan19
```
You must set the path to your cplex directory in `package.yaml`. You can use this command:
```
sed -i 's,/home/sat/cplex2210,/path/to/cplex,g' package.yaml

```
Run the following commands to install

```
stack build
stack install
```

On Linux, the binary will be copied in `~/.local/bin/TamVan19-exe`. Please, refer to the documentation of `stack` for *Windows* or *MacOS*.

## Generating the nondominated set

Simply run the command 
```
syntax: ~/local/bin/TanVan19-exe instancetype conf i instance logfile

```
where

- `instancetype` is either `KP`, `AP`, `MODO` depending of the instance file syntax. See below.
- `conf` is either `twostage` or `scal` depending if you want to use the *two-stage* approach or the *direct* approach. The *direct* approach is faster but may lead to a superset of the nondominated set which contains some weakly dominated points.
- `i` is an identifier of the instance which will appear in the log file. Personally, `i` is the index of the instance in the dataset.
- `instance` is the path to the instance file
- `logfile` is the path of the file where the statistics of the execution will be append to. See below.

Example:
```
~/.local/bin/TanVan19-exe KP twostage 1 Instances/MOKP/MOKP_p-2_n-200_1.dat test.log
```
The nondominated set can be found in the file `ndpts`.

## Log file syntax
The log file is a *CSV* file containing differents metrics. The columns are:
- $n$,$p$ the parameters of the instance file (see below)
- `i` the identifier of the instance specified above
- the type of approach
- the CPU runtime
- the number of nondominated points
- the number of iterations *i.e.* the number of zones that have been explored.
- the number of infeasible models *i.e.* the number of empty zones that have been explored. Should be equal to zero except if the problem is infeasible.
- the number of redundant points *i.e.* the number of models which leads to a point that have already been found.
- the maximum size of the search region *i.e* the maximum number of remaining zones to be explored during the enumeration.
- the average size of the search region *i.e* the average number of remaining zones to be explored during the enumeration.


## Instance file type
Instances files are descripted using three types of lines:

- Integers
- List of doubles using the haskell syntax, *e.g.* :  
```haskell 
[2,5,3.0]
```
This will be referred to as `[Double]`
- List of list of doubles, *e.g.*:
```haskell
[[2,5,3], [4,2,1], [5,3,1]]
```
This will be referred to as `[[Double]]`

Note that lists must be written **on a single line** to be properly parsed. Moreover, **no empty lines** are allowed. Sorry for this strict parser, I may relax theses constraints at some point.

### Multiobjective knapsack
We consider a knapsack problem of $n$ items and $p$ objective functions.
- The two first lines contain the number of objectives and the number of items, *i.e.* $p$ and $n$.
- Then we have the maximum capacity of the knapsack.
- Then we have the list of $p$ objective functions, each one being a list of $n$ doubles. Thus, the type of this line is `[[Double]]`.
- Finally, we have the weights of each items which is a list of $n$ doubles. Thus, the type of this line is `[Double]`.

Examples are provided in `Instances/MOKP`.

### Multiobjective assigment
We consider here an assignment problem of $n$ workers to $n$ machines according to $p$ objective functions. In this situation, an objective function is a matrix $C \in M_{n\times n} (\mathbb{N})$ where $C_{ij}$ corresponds to the cost of assigning the worker $i$ to the machine $j$.
- The two first lines contain the number of objectives and the number of workers to be assigned, *i.e.* $p$ and $n$.
- The next line contains the list of $p$ objective functions, each one being a matrix *i.e.* a list of $n$ list of $n$ doubles. Thus, the type of this line is `[[[Double]]]`.

Examples are provided in `Instances/MOAP`.

### Generic Multiobjective Discrete Optimization Problem (experimental)
A generic domain in the form

$$
\left\\{\begin{array}{ll}
\min & \mathbf{f(x)}=[f_1(x),\ldots,f_p(x)]^T \\
\mbox{s.t.} & \mathbf{l} \preceq A\mathbf{x} \preceq \mathbf{u} \\
            & \mathbf{x} \in \\{0,1\\}^n 
\end{array}\right.
$$
- The two first lines contain the number of objectives and the number of (binary) variables, *i.e* $p$ then $n$.
- The next line contains $\textbf{f}$ represented as a list of p objective functions, each one being a list of doubles. Thus, this line is of type `[[Double]]`.
- The next line contains the lower bounds $\mathbf{l}$ represented as a list of $m$ doubles. Thus, this line is of type `[Double]`.
- The next line contains the constraint matrix $A$ represented as a list of $m$ constraints, each one being a list of $n$ doubles. Thus, this line is of type `[[Double]]`.
- The last line contains the upper bounds $\mathbf{u}$ represented as a list $m$ doubles. Thus, this line is of type `[Double]`.


As an example, `Instances/MODO/test.dat` contains the following dummy tri-objective problem:


$$
\left\\{\begin{array}{ll}
\min & f_1(\mathbf{x}) = x_1 + 2x_2 + 3x_3 + 4x_4 + 5x_5 \\
     & f_2(\mathbf{x}) = 5x_1 + 4x_2 + 3x_3 + 2x_4 + x_5 \\
     & f_3(\mathbf{x}) = 2x_1 + x_2 + 3x_3 + 4x_4 + x_5  \\
\mbox{s.t.} & 5 \le 5x_1+2x_2+x_3+7x_4+3x_5 \le 10 \\
            & 5 \le 5x_1 + 10x_2 + 3x_3 +4x_4 + 9x_5 \le 20 \\
            & [x_1,x_2,x_3,x_4,x_5]^T \in \\{0,1\\}^5
\end{array}\right.
$$

Please, report any problem with this parser !

## Citation
If you use this code, please cite the paper:
```
@article{tamby2021enumeration,
  title={Enumeration of the nondominated set of multiobjective discrete optimization problems},
  author={Tamby, Satya and Vanderpooten, Daniel},
  journal={INFORMS Journal on Computing},
  volume={33},
  number={1},
  pages={72--85},
  year={2021},
  publisher={INFORMS}
}
```
