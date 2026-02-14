# ACL Principal

An ACL principal (user, group, or role)

## Public fields

- `type`:

  Principal type

- `value`:

  Principal value

## Methods

### Public methods

- [`ACLPrincipal$new()`](#method-ACLPrincipal-new)

- [`ACLPrincipal$matches()`](#method-ACLPrincipal-matches)

- [`ACLPrincipal$to_string()`](#method-ACLPrincipal-to_string)

- [`ACLPrincipal$clone()`](#method-ACLPrincipal-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new ACLPrincipal

#### Usage

    ACLPrincipal$new(type, value)

#### Arguments

- `type`:

  Principal type (user, group, role)

- `value`:

  Principal value

------------------------------------------------------------------------

### Method `matches()`

Check if this principal matches another

#### Usage

    ACLPrincipal$matches(other)

#### Arguments

- `other`:

  Another ACLPrincipal

#### Returns

Logical

------------------------------------------------------------------------

### Method `to_string()`

Convert to string

#### Usage

    ACLPrincipal$to_string()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ACLPrincipal$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
