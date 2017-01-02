module NonEmptyList exposing (NonEmptyList)


type NonEmptyList a
    = NonEmptyList a (List a)
