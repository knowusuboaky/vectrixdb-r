# ACL Configuration

ACL configuration for a document or collection

## Public fields

- `read_principals`:

  Who can read

- `deny_principals`:

  Who cannot read (takes precedence)

- `is_public`:

  Is public access allowed

## Methods

### Public methods

- [`ACLConfig$new()`](#method-ACLConfig-new)

- [`ACLConfig$clone()`](#method-ACLConfig-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new ACLConfig

#### Usage

    ACLConfig$new(
      read_principals = list(),
      deny_principals = list(),
      is_public = FALSE
    )

#### Arguments

- `read_principals`:

  List of ACLPrincipal objects

- `deny_principals`:

  List of ACLPrincipal objects

- `is_public`:

  Logical

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ACLConfig$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
