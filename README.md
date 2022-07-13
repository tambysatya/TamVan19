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

### Multiobjective knapsack
### Multiobjective assigment

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
