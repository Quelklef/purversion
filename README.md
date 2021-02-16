# Purversion

Richly-typed and versioned localStorage keys for Purescript

### Super Simple API

1. Register your localStorage key
   ```purescript
   pv <- Purversion.make
         { key: "a-localstorage-key"  -- key name
         , encode: intToString        -- data encoding. we're storing integers
         , decode: stringToInt
         , migrations: [Identity]     -- data migrations, if applicable
         }
   ```
   Note that the monad is just `Effect`, nothing special.

2. Save and load values
   ```purescript
   -- Put a value into the key
   Purversion.save pv 100
   
   -- Get a value out of the key
   value <- Purversion.load pv
   log $ show value  -- 100
   ```

3. No step 3. That's it!

### Documentation

- See [test/Example.purs](https://github.com/Quelklef/purversion/blob/master/test/Example.purs) or [test/Main.purs](https://github.com/Quelklef/purversion/blob/master/test/Main.purs) for examples
- See [Pursuit](https://pursuit.purescript.org/packages/purescript-purversion) for hard documentation

Enjoy!
