# Description

The goal of this project is to inject components between each wiring end of the main application's components in order to observe the states and transitions of interfaces.

## Run

* Install TinyOS
* Pass .nc file to the application
  - `runhaskell -isrc src/Main.hs $(TOSDIR)/apps/Blink/BlinkAppC.nc`

## Steps

- [ ] Parse interfaces
- [ ] Generate connector components for each wiring
- [ ] Parse application and use connector components

## Tests

`runhaskell -isrc test/ParserSpec.hs`
