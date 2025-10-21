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
    , attr (AttrName "d") "M 10 30 L 18 30 L 18 28 L 15 26 L 15 2 L 13 2 L 13 26 L 10 28 z"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "flag"
    , attr (AttrName "d") "M 13 2 L 15 2 L 28 6 L 15 10 L 13 10 z"
    ]
    [ 
    ]
  ]

iconMine :: forall p r i. Array (IProp r i) -> HTML p i
iconMine attrs =
  elementNS ns (ElemName "g")
  ( attrs <> [ ])
  [ elementNS ns (ElemName "rect")
    [ attr (AttrName "class") "mine-background"
    , attr (AttrName "width") "32"
    , attr (AttrName "height") "32"
    ]
    [ 
    ], elementNS ns (ElemName "circle")
    [ attr (AttrName "class") "mine"
    , attr (AttrName "cx") "16"
    , attr (AttrName "cy") "16"
    , attr (AttrName "r") "10"
    ]
    [ 
    ], elementNS ns (ElemName "g")
    [ attr (AttrName "transform") "translate(16, 16)"
    ]
    [ elementNS ns (ElemName "g")
      [ attr (AttrName "class") "mine-wobble"
      ]
      [ elementNS ns (ElemName "rect")
        [ attr (AttrName "class") "mine"
        , attr (AttrName "x") "-2"
        , attr (AttrName "y") "8"
        , attr (AttrName "width") "4"
        , attr (AttrName "height") "5"
        , attr (AttrName "transform") "rotate(0)"
        ]
        [ 
        ], elementNS ns (ElemName "rect")
        [ attr (AttrName "class") "mine"
        , attr (AttrName "x") "-2"
        , attr (AttrName "y") "8"
        , attr (AttrName "width") "4"
        , attr (AttrName "height") "5"
        , attr (AttrName "transform") "rotate(60)"
        ]
        [ 
        ], elementNS ns (ElemName "rect")
        [ attr (AttrName "class") "mine"
        , attr (AttrName "x") "-2"
        , attr (AttrName "y") "8"
        , attr (AttrName "width") "4"
        , attr (AttrName "height") "5"
        , attr (AttrName "transform") "rotate(120)"
        ]
        [ 
        ], elementNS ns (ElemName "rect")
        [ attr (AttrName "class") "mine"
        , attr (AttrName "x") "-2"
        , attr (AttrName "y") "8"
        , attr (AttrName "width") "4"
        , attr (AttrName "height") "5"
        , attr (AttrName "transform") "rotate(180)"
        ]
        [ 
        ], elementNS ns (ElemName "rect")
        [ attr (AttrName "class") "mine"
        , attr (AttrName "x") "-2"
        , attr (AttrName "y") "8"
        , attr (AttrName "width") "4"
        , attr (AttrName "height") "5"
        , attr (AttrName "transform") "rotate(240)"
        ]
        [ 
        ], elementNS ns (ElemName "rect")
        [ attr (AttrName "class") "mine"
        , attr (AttrName "x") "-2"
        , attr (AttrName "y") "8"
        , attr (AttrName "width") "4"
        , attr (AttrName "height") "5"
        , attr (AttrName "transform") "rotate(300)"
        ]
        [ 
        ]
      ]
    ]
  ]

iconUnclicked :: forall p r i. Array (IProp r i) -> HTML p i
iconUnclicked attrs =
  elementNS ns (ElemName "g")
  ( attrs <> [ ])
  [ elementNS ns (ElemName "path")
    [ attr (AttrName "class") "light"
    , attr (AttrName "d") "M 32 0 L 30 2 L 2 2 L 2 30 L 0 32 L 0 0 z"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "shadow"
    , attr (AttrName "d") "M 32 0 L 30 2 L 30 30 L 2 30 L 0 32 L 32 32z"
    ]
    [ 
    ]
  ]

