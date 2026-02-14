# CLI Configuration

Configuration for CLI behavior

## Public fields

- `verbose`:

  Print verbose output

- `color`:

  Use colored output

- `data_dir`:

  Default data directory

## Methods

### Public methods

- [`CLIConfig$new()`](#method-CLIConfig-new)

- [`CLIConfig$clone()`](#method-CLIConfig-clone)

------------------------------------------------------------------------

### Method `new()`

Create CLI config

#### Usage

    CLIConfig$new(verbose = TRUE, color = TRUE, data_dir = "./vectrixdb_data")

#### Arguments

- `verbose`:

  Verbose output

- `color`:

  Colored output

- `data_dir`:

  Data directory

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CLIConfig$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
