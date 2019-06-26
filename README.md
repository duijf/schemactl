# schemactl - Control your PostgreSQL schema

Dev and operator friendly PostgreSQL schema migration tool.

## Goals

Schemactl aims to be really friendly to **both** developers writing migrations
and operators applying them.

Developers want quality of life features which help them write migrations and
test them quickly. Operators have a primary interest in the safety of
migrations and making this process as uneventful as possible.

As with all tools which seek to find a balance between (seemingly) different
goals: perfection is difficult. There will be situations where the tool is a
bad fit or certain users wished for a different trade-off. I'll try to be
honest about these compromises and [will inform you][compromises] of them when
they arise.

The following list are are some of the things the author considers Good Things
when it comes to schema migrations. If you find yourself nodding in agreement,
it might be worth checking out Schemactl in more detail.

 - **Feature completeness.** Support all Postgres features. Don't obscure SQL
   through some other DSL.
 - **Standalone.** Do not be coupled to language specific ORMs or database
   libraries. This allows you to standardize tooling for polyglot
   projects/architectures.
 - **Linearizable history.** Have a set and deterministic order in which
   migrations are applied.
 - **Immutability.** Once a migration has been applied to a DB, you cannot
   change it's code (barring explicit escape hatches).

 [compromises]: #compromises-and-rough-edges

## Planned features

 - Use plain SQL files as migration scripts.
 - Run migrations in transactions automatically, but provide more granular
   control when required.
 - Keep a log of applied migrations and metadata about those changes.
 - Support a dry run mode where you'll be able to see what code will run.
 - CI tool to check whether upgrades and downgrades are each other's inverse.
 - A wide array of connection options and aliases. Use with multiple databases
   or clusters. Support SSH tunnels.
 - Locking and freezing of migrations.

## Compromises and rough edges

There is no code yet. On the bright side: the bug count is the lowest it will
ever be.
