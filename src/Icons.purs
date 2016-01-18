module Icons where

import Prelude ((<>))
import Halogen.HTML

ns :: Namespace
ns = Namespace "http://www.w3.org/2000/svg"


iconFlag :: forall p r i. Array (IProp r i) -> HTML p i
iconFlag attrs =
  elementNS ns (ElemName "g")
  ( attrs <> [ ])
  [ elementNS ns (ElemName "path")
    [ attr (AttrName "class") "flagpole"
    , attr (AttrName "fill") "black"
    , attr (AttrName "d") "M 10 30 L 18 30 L 18 28 L 15 26 L 15 2 L 13 2 L 13 26 L 10 28 z"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "flag"
    , attr (AttrName "fill") "red"
    , attr (AttrName "d") "M 13 2 L 15 2 L 28 6 L 15 10 L 13 10 z"
    ]
    [ 
    ]
  ]

iconUnclicked :: forall p r i. Array (IProp r i) -> HTML p i
iconUnclicked attrs =
  elementNS ns (ElemName "g")
  ( attrs <> [ ])
  [ elementNS ns (ElemName "path")
    [ attr (AttrName "class") "light"
    , attr (AttrName "fill") "white"
    , attr (AttrName "d") "M 32 0 L 30 2 L 2 2 L 2 30 L 0 32 L 0 0 z"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "shadow"
    , attr (AttrName "fill") "darkgrey"
    , attr (AttrName "d") "M 32 0 L 30 2 L 30 30 L 2 30 L 0 32 L 32 32z"
    ]
    [ 
    ]
  ]

