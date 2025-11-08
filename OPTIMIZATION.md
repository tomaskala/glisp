- NaN boxing
- Store globals in an array instead of a map
- Run-length encoding of the line numbers in a chunk
- Get rid of runtime.Upvalue.IsClosed - set the Index to a negative value 
  instead
- Get rid of runtime.Function.HasRestParam - set the Arity to a negative value 
  instead
- Get rid of runtime.UpvalueSpec.IsLocal - set the Index to a negative value 
  instead
