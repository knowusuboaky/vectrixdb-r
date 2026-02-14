# Simple Community Detector

Detects communities using connected components

## Public fields

- `min_size`:

  Minimum community size

- `max_levels`:

  Maximum hierarchy levels

## Methods

### Public methods

- [`CommunityDetector$new()`](#method-CommunityDetector-new)

- [`CommunityDetector$detect()`](#method-CommunityDetector-detect)

- [`CommunityDetector$clone()`](#method-CommunityDetector-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new CommunityDetector

#### Usage

    CommunityDetector$new(min_size = 5, max_levels = 3)

#### Arguments

- `min_size`:

  Min size

- `max_levels`:

  Max levels

------------------------------------------------------------------------

### Method `detect()`

Detect communities in graph

#### Usage

    CommunityDetector$detect(graph)

#### Arguments

- `graph`:

  KnowledgeGraph object

#### Returns

List of Community objects

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CommunityDetector$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
