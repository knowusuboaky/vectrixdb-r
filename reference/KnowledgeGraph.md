# Knowledge Graph

Graph storage for entities and relationships

## Public fields

- `name`:

  Graph name

## Methods

### Public methods

- [`KnowledgeGraph$new()`](#method-KnowledgeGraph-new)

- [`KnowledgeGraph$add_entity()`](#method-KnowledgeGraph-add_entity)

- [`KnowledgeGraph$add_relationship()`](#method-KnowledgeGraph-add_relationship)

- [`KnowledgeGraph$get_entity()`](#method-KnowledgeGraph-get_entity)

- [`KnowledgeGraph$get_all_entities()`](#method-KnowledgeGraph-get_all_entities)

- [`KnowledgeGraph$get_all_relationships()`](#method-KnowledgeGraph-get_all_relationships)

- [`KnowledgeGraph$get_neighbors()`](#method-KnowledgeGraph-get_neighbors)

- [`KnowledgeGraph$traverse()`](#method-KnowledgeGraph-traverse)

- [`KnowledgeGraph$entity_count()`](#method-KnowledgeGraph-entity_count)

- [`KnowledgeGraph$relationship_count()`](#method-KnowledgeGraph-relationship_count)

- [`KnowledgeGraph$search_entities()`](#method-KnowledgeGraph-search_entities)

- [`KnowledgeGraph$clone()`](#method-KnowledgeGraph-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new KnowledgeGraph

#### Usage

    KnowledgeGraph$new(name = "default")

#### Arguments

- `name`:

  Graph name

------------------------------------------------------------------------

### Method `add_entity()`

Add an entity

#### Usage

    KnowledgeGraph$add_entity(entity)

#### Arguments

- `entity`:

  Entity object

------------------------------------------------------------------------

### Method `add_relationship()`

Add a relationship

#### Usage

    KnowledgeGraph$add_relationship(relationship)

#### Arguments

- `relationship`:

  Relationship object

------------------------------------------------------------------------

### Method `get_entity()`

Get entity by ID

#### Usage

    KnowledgeGraph$get_entity(entity_id)

#### Arguments

- `entity_id`:

  Entity ID

#### Returns

Entity or NULL

------------------------------------------------------------------------

### Method `get_all_entities()`

Get all entities

#### Usage

    KnowledgeGraph$get_all_entities()

#### Returns

List of Entity objects

------------------------------------------------------------------------

### Method `get_all_relationships()`

Get all relationships

#### Usage

    KnowledgeGraph$get_all_relationships()

#### Returns

List of Relationship objects

------------------------------------------------------------------------

### Method `get_neighbors()`

Get neighbors of an entity

#### Usage

    KnowledgeGraph$get_neighbors(entity_id, direction = "both")

#### Arguments

- `entity_id`:

  Entity ID

- `direction`:

  "out", "in", or "both"

#### Returns

List of Entity objects

------------------------------------------------------------------------

### Method `traverse()`

Traverse graph from seed entities

#### Usage

    KnowledgeGraph$traverse(seed_ids, max_depth = 2)

#### Arguments

- `seed_ids`:

  Starting entity IDs

- `max_depth`:

  Maximum depth

#### Returns

SubGraph object

------------------------------------------------------------------------

### Method `entity_count()`

Get entity count

#### Usage

    KnowledgeGraph$entity_count()

#### Returns

Integer

------------------------------------------------------------------------

### Method `relationship_count()`

Get relationship count

#### Usage

    KnowledgeGraph$relationship_count()

#### Returns

Integer

------------------------------------------------------------------------

### Method `search_entities()`

Search entities by name

#### Usage

    KnowledgeGraph$search_entities(query, limit = 10)

#### Arguments

- `query`:

  Query string

- `limit`:

  Max results

#### Returns

List of Entity objects

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    KnowledgeGraph$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
