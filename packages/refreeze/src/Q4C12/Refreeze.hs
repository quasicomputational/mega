module Q4C12.Refreeze
  ( RefreezeConfig
  , defaultRefreezeConfig
  , projects
  , refreezeSchema
  )
  where

import qualified Data.Text.Lazy as LT
import Q4C12.XML
  ( uname
  )
import Q4C12.XMLDesc
  ( Desc
  , El
  , consolidate
  , elementMixed
  , flowEvenPreWSDropComments
  , flowWSEDropComments
  , oddTxNoPos
  , rfmap
  , rmany
  )

data RefreezeConfig = RefreezeConfig
  { _refreezeProjects :: [ FilePath ]
  }

defaultRefreezeConfig :: RefreezeConfig
defaultRefreezeConfig = RefreezeConfig []

projects :: Lens' RefreezeConfig [ FilePath ]
projects f ( RefreezeConfig ps ) = RefreezeConfig <$> f ps

refreezeSchema
  :: (Desc tag)
  => El tag RefreezeConfig
refreezeSchema
  = rfmap (iso f g) $ elementMixed ( uname "refreeze" ) $ flowEvenPreWSDropComments
  $ rmany
  $ flowWSEDropComments
  $ rfmap ( iso LT.unpack LT.pack )
  $ elementMixed ( uname "build" )
  $ rfmap consolidate
  $ oddTxNoPos
  where
  f = RefreezeConfig
  g ( RefreezeConfig ps ) = ps
