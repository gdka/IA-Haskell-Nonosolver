# Nonosolver - Nonogram Solver

* 13-10665 Augusto Hidalgo
* 13-10708 Genesis Kufatty
* 13-11310 Mathias San Miguel


## How to build

You should compile `nonosolver.hs` with GHC. The `minisat` binary must be in the PATH, or the current directory, so that nonosolver can find it. All the minisat output goes to stdout (and can be redirected to a `minisat.log` file, if necessary).


## How to use

The solver accepts nonograms in a slightly simplified version of the `.non` format defined by Steve Simpson. If the program is ran without arguments, it reads from stdin.

```bash
./nonosolver
```

You can optionally tell the program which `.non` file to read via the `-non` flag.

```bash
./nonosolver -non "some/path/to/file.non"
```

In the same way, you can tell it which files to use for the different outputs: 

```bash
./nonosolver -cnf "sat_model.cnf" -res "solution_to_sat.txt" -pbm "image.pbm"
```

You can ask for none or any combination of them, as they have default values and behavior.


### Utility 

As there are some test cases, the script `runAll.sh` runs the solver on every `.non` file in `the nonogramas` folder, in a sequential manner. **This script must be run from the folder containing it**. Output is placed in the `res` folder (subdivided in `cnf`, `res`, `pbm` folders for output files and usually stdout-ed minisat output is redirected to log files in `res/minisatOutput`.


## How it works

> For now, explained in `proyecto-3/informe.md`
