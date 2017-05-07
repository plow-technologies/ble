# ble - Bluetooth Low Energy for Haskell

*ble* is a Haskell library for writing Bluetooth Low Energy peripherals (and
soon centrals).

For usage, see the  [haddocks](https://hackage.haskell.org/package/ble). There
are also examples in
[`examples`](https://github.com/plow-technologies/ble/tree/master/examples)
directory.

## Requirements

`ble` currently only supports Linux, and requires Bluez versions 5.41 and up.
To see what version you are running, type:

``` bash
bluetoothd --version
```
