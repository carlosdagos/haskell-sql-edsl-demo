{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module OpaleyeDemo.Hashtag where

import Control.Arrow
       ( returnA )
import Data.Time.Calendar
       ( Day )
import Opaleye
       ( Table(..), Query, QueryArr, Column, Nullable, PGInt4, PGText, PGDate
       , queryTable, required, optional, restrict, (.==) )
import Data.Profunctor.Product.TH
       ( makeAdaptorAndInstance )


