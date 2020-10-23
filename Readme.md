[![Githbu actions build status](https://img.shields.io/github/workflow/status/jappeace/haskell-template-project/Test)](https://github.com/riskbook/dropbox-client/actions)

> It doesn't actually replace a USB drive.
>
> - BrandomM on hackernews, commenting on the dropbox idea, 2007

This is an incomplete dropbox API client derived from a servant
generic type definition.
Additional endpoints are welcome, feel free to make a PR!

## History
This is the result from the great riskbook hackathon
of oktober 2020
(also known as festoberhack).

## Usage

### Tools
Enter the nix shell.
```
nix-shell
```
You can checkout the makefile to see what's available:
```
cat makefile
```

### Running
```
make run
```

### Fast filewatch which runs tests
```
make ghcid
```
