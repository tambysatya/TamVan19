# TamVan19 implementation

This tool is an implementation of the paper [Enumeration of the nondominated set of multiobjective discrete optimization problems, by S. Tamby and D. Vanderpooten](https://pubsonline.informs.org/doi/abs/10.1287/ijoc.2020.0953).

## Disclaimer

Note that this code has **not** been used to perform the computational experiments of the paper. The proper version is available at the URL provided in the paper, which must have been linked with an older IBM Cplex (c) version.

## Installation

Install [the haskell tool stack](https://docs.haskellstack.org/) a reproducible building environment in haskell. Note that this tool is packaged in most public linux distribution and can be installed using the standard package manager.

Clone this repository

```
git clone https://github.com/tambysatya/TamVan19.git
cd TamVan19
```
**You must set the path to your cplex directory in `package.yaml`** by using this command:
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
Please cite as
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
