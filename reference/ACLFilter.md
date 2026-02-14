# ACL Filter

Access Control List filter for security-aware search

## Public fields

- `acl_field`:

  Metadata field containing ACLs

## Methods

### Public methods

- [`ACLFilter$new()`](#method-ACLFilter-new)

- [`ACLFilter$filter()`](#method-ACLFilter-filter)

- [`ACLFilter$add_acl()`](#method-ACLFilter-add_acl)

- [`ACLFilter$create_filter_condition()`](#method-ACLFilter-create_filter_condition)

- [`ACLFilter$clone()`](#method-ACLFilter-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new ACLFilter

#### Usage

    ACLFilter$new(acl_field = "_acl")

#### Arguments

- `acl_field`:

  Field name for ACLs (default: "\_acl")

------------------------------------------------------------------------

### Method [`filter()`](https://rdrr.io/r/stats/filter.html)

Filter documents based on user's ACL principals

#### Usage

    ACLFilter$filter(documents, user_principals, default_allow = FALSE)

#### Arguments

- `documents`:

  List of documents with metadata

- `user_principals`:

  Character vector or list of ACLPrincipal

- `default_allow`:

  Allow if no ACL defined (default: FALSE)

#### Returns

Filtered documents

------------------------------------------------------------------------

### Method `add_acl()`

Add ACL to document metadata

#### Usage

    ACLFilter$add_acl(metadata, principals)

#### Arguments

- `metadata`:

  Document metadata

- `principals`:

  Character vector of principal strings

#### Returns

Updated metadata

------------------------------------------------------------------------

### Method `create_filter_condition()`

Create ACL filter condition for query

#### Usage

    ACLFilter$create_filter_condition(user_principals)

#### Arguments

- `user_principals`:

  Character vector of principals

#### Returns

Filter condition list

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ACLFilter$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
acl_filter <- ACLFilter$new()
filtered <- acl_filter$filter(
  documents = results,
  user_principals = c("user:alice", "group:engineering")
)
} # }
```
