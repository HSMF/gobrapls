# Gobrapls

LSP for Gobra

## Installation
```sh
cargo install --git https://github.com/HSMF/gobrapls
```

Requires a more or less recent version of Rust

### Neovim

Place the following in `~/.config/nvim/after/ftplugin/go.lua` and `~/.config/nvim/after/ftplugin/gobra.lua`

- TODO filetype detection for gobra

```lua
vim.lsp.start({
    name = "gobrapls",
    cmd = {
        "gobrapls",
        "--java",
        -- this should be the path to a java 11 executable
        "/usr/lib/jvm/java-11-openjdk/bin/java",
        "--gobra",
        "/path/to/gobra/",
        -- optionally specify additional flags to gobra
        -- "--gobraflags",
        -- " ",
    },
    root_dir = vim.fs.root(0, { "go.sum", "go.mod" }),
})
```

### vscode

TODO check out vscode


## Lints

- contract_order.scm
  - ensures that pre-conditions are placed before post-conditions

- slice_index_trigger.scm
  - checks that triggers use `&s[i]` instead of `s[i]`.
  - this is because `&s[i]` is a more general trigger than `s[i]`
  - TODO: check that s is addressable and only warn if it is

- TODO: no unfolding in contracts

- TODO: 


## Completion

- TODO

## Document Symbol

- resolves functions and predicates of file
- TODO: symbols of other files
