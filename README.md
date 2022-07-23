# TamVan19 implementation

This tool is an implementation of the paper [Enumeration of the nondominated set of multiobjective discrete optimization problems, by S. Tamby and D. Vanderpooten](https://pubsonline.informs.org/doi/abs/10.1287/ijoc.2020.0953) written in Haskell.

## Disclaimer



Note that this code has **not** been used to perform the computational experiments of the paper. The proper version is available at the URL provided in the paper, which must have been linked with an older IBM Cplex (c) version. This code has been updated to be used with **IBM Cplex(c) 22.10**.

As you can see in the source code, I am not an expert developper. Despite the reliability of Haskell, this code is provided without any waranty so use it at your own risks. Please do not hesitate to submit any bug.

This code has been tested on linux (*NixOS 22.05*), but should work on other distributions. Please report any problem, especially on *windows* or *macOS*.

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

The binary will be copied in `~/.local/bin/TamVan19-exe`

## Generating the nondominated set

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
### Multiobjective assigment
### Generic Multiobjective Discrete Optimization Problem (experimental)
A standard domain the form

$$
\left\\{\begin{array}{ll}
\min & \mathbf{f(x)}=[f_1(x),\ldots,f_p(x)]^T \\
\mbox{s.t.} & \mathbf{l} \preceq A\mathbf{x} \preceq \mathbf{u} \\
            & \mathbf{x} \in \\{0,1\\}^n 
\end{array}\right.
$$
- The two first lines contains the number of objectives and the number of (binary) variables, *i.e* $p$ then $n$.
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
