module System where

type BString = [Int]
type BChar   = Int
type BTime   = Int
type BDate   = Int
type BFile   = Int
type BVector = Int
type BInt    = Int
type BError  = Int
type BVoid   = Int
type BOffset = Int
type BMode   = Int

data ServiceCalls m = ServiceCalls
  { service_putchar :: BChar -> m BVoid
  , service_getchar :: m BChar
  , service_open    :: BString -> BMode -> m BFile
  , service_close   :: BFile -> m BError
  , service_read    :: BFile -> BInt -> m (BInt, [BInt])
  , service_write   :: BFile -> BString -> BInt -> m BInt
  , service_seek    :: BFile -> BOffset -> BInt -> m BInt
  , service_creat   :: BString -> BMode -> m BFile
  , service_unlink  :: BString -> m BError
  , service_time    :: m (BInt,BInt)
  , service_wait    :: m BError
  , service_exit    :: m BVoid
  , service_fork    :: m BError
  , service_execv   :: BString -> [BString] -> BInt -> m BError
  }

dummySystem :: Applicative m => ServiceCalls m
dummySystem = ServiceCalls
  { service_putchar = \_ -> pure 0
  , service_getchar = pure 4
  , service_open    = \_ _ -> pure 0
  , service_close   = \_ -> pure 0
  , service_read    = \_ _ -> pure (0, [])
  , service_write   = \_ _ _ -> pure 0
  , service_seek    = \_ _ _ -> pure 0
  , service_creat   = \_ _ -> pure 0
  , service_unlink  = \_ -> pure 0
  , service_time    = pure (0,0)
  , service_wait    = pure 0
  , service_exit    = pure 0
  , service_fork    = pure 0
  , service_execv   = \_ _ _ -> pure 0
  }
  
