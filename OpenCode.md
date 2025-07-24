# Haskell Project Guidelines for OpenCode

This document outlines the conventions and commands for the `purescript-mcp-server` Haskell project.

## Nix Commands

- **Build with Nix**: `nix build`
- **Enter Nix develop shell**: `nix develop`

## Build/Lint/Test Commands

- **Build all components**: `cabal build all`
- **Run all tests**: `cabal test all`
- **Run a single test**: `cabal test all --test-options="--match <test_name>"`
  - Replace `<test_name>` with the full description of the test case you want to run. You can find this in the test suite output or the `test/Spec/` modules.
- **Linting/Typechecking**: `cabal build all -fno-code`
  - This command will perform type-checking and report any warnings (linting) defined by the `-Wall` GHC option in the `.cabal` file.

## Code Style Guidelines

- **Imports**: Organize imports alphabetically and group them (e.g., standard library, third-party, local modules).
- **Formatting**: Adhere to the `ormolu` formatting style. The `cabal` build process is configured with `-Wall` to catch many common issues.
- **Types**: Utilize Haskell's strong type system to define clear and precise types for functions and data structures. Type signatures are encouraged for all top-level definitions.
- **Naming Conventions**:
    - `camelCase` for functions, variables, and type variables.
    - `PascalCase` for data constructors and type names.
    - Type class and module names should also be `PascalCase`.
- **Error Handling**: Prefer using `Either` or `Maybe` for recoverable errors, and `IO` for side-effecting operations that might throw exceptions. Avoid `error` or `undefined` where possible for production code.
- **Purity**: Strive for pure functions where appropriate, separating pure logic from impure side-effects.
- **Documentation**: Provide Haddock comments for all exported functions, types, and modules.
