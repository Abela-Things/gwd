# TYPE SYSTEM

In gwd, Geneweb data are converted into Jingoo's type system.

NB: Most of the time, fields can be undefined (i.e. represented by [null]).

For more informations about [jingoo](https://geneanet.github.io/jingoo/ocaml),
see [https://geneanet.github.io/jingoo/ocaml].

```
type 'a pat
(** An object of type ['a]. ([Jingoo.Jg_types.Tpat]) *)

type 'a assoc
(** An [(string * 'a)] associative list. ([Jingoo.Jg_types.Tobj]) *)

type str
(** A string. ([Jingoo.Jg_types.Tstr]) *)

type bool
(** A boolean. ([Jingoo.Jg_types.Tbool]) *)

type 'a list
(** An list of ['a]. ([Jingoo.Jg_types.Tlist] or [Jingoo.Jg_types.Tarray]) *)

type int
(** An int. ([Jingoo.Jg_types.Tint]) *)

type float
(** An float. ([Jingoo.Jg_types.Tfloat]) *)

type 'a func
(** A function. ([Jingoo.Jg_types.Tfun]) *)

type 'a mandatory
(** An ['a] which can not be [null] *)
```

## Families and persons

### Family

```
type family =
  { divorce_date : date
  ; children : person list
  ; father : person
  ; ifam : int mandatory
  ; marriage_date : date
  ; marriage_place : str
  ; marriage_note : str
  ; marriage_source : str
  ; mother : person
  ; origin_file : str
  ; relation : str
  ; separation : str
  ; spouse : person
  ; witnesses : person list
  }
```

A `family` is two parents, and 0 or more children.

A few notes about the field of `family` structure:

- `ifam` is the `family` unique identifier.
- When the `family` is fetch from the field of a parent, `spouse` field
  is set to the other parent of this family.
  i.e. `mom.families[0].spouse = mom.families[0].father`.
- `relation` is `null`, `"MARRIED"`, `"NOT_MARRIED"`, `"ENGAGED"`,
  `"NO_SEXES_CHECK_NOT_MARRIED"`, `"NO_SEXES_CHECK_MARRIED"`, `"MARRIAGE_BANN"`,
  `"MARRIAGE_CONTRACT"`, `"MARRIAGE_LICENSE"`, `"PACS"` or `"RESIDENCE"`
- `separation` is `null`, `"DIVORCED"` or `"SEPARATED"`

### Person

```
type person =
  { access : str
  ; baptism_date : date
  ; baptism_place : str
  ; birth_date : date
  ; birth_place -> str
  ; burial : burial
  ; burial_place : str
  ; children : person list
  ; cremation_place : str
  ; consanguinity : float
  ; dates : str
  ; death : death
  ; death_place : str
  ; digest : str
  ; events : event list
  ; families : family list
  ; father : person
  ; first_name : str
  ; first_name_aliases : str list
  ; first_name_key : str
  ; first_name_key_val : str
  ; image_url : str
  ; iper : int mandatory
  ; is_birthday : bool
  ; linked_page : (str -> str) func
  ; max_ancestor_level : int
  ; mother : person
  ; nb_families : int
  ; occ : int
  ; occupation : str
  ; public_name : str
  ; qualifier : str
  ; qualifiers : str list
  ; relations : relation list
  ; related : relation list
  ; sex : int
  ; siblings : person list
  ; source_baptism : str
  ; source_birth : str
  ; source_burial : str
  ; source_death : str
  ; source_fsource : str
  ; source_marriage : str
  ; source_psources : str
  ; spouses : person list
  ; surname : str
  ; surname_aliases : str list
  ; surname_key : str
  ; surame_key_val : str
  ; titles : title list
  ; __str__ : str
  }
```

```
type title =
  { ident : str
  ; name : str
  ; place : str
  ; date_start : date
  ; date_end : date
  ; nth : int
  }
```

### Date/dmy

A `date` is either a `date_` or a `textual_date`

```
type date_ =
  { calendar : str
  ; d2 : date
  ; day : int
  ; month : int
  ; prec : str
  ; year : int
  ; __compare__ : (date -> date -> int) func
  ; __eq__ : (date -> date -> bool) func
  }
```

- `calendar` can be: `"Dgregorian"`, `"Djulian"`, `"Dfrench"` or `"Dhebrew"`
- `prec` can be: `"sure"`, `"about"`, `"maybe"`, `"before"`, `"after"`, `"oryear"` or `"yearint"`


```
type dmy =
  { day : int
  ; delta : int
  ; month : int
  ; year : int
  }
```

```
type textual_date =
  { __str__ : str
  ; __compare__ : date -> date -> int
  ; __eq__ : date -> date -> bool
  ; __Dtext__ : bool
 }
```

Textual dates are considered as equal with any other value when
using `__compare__` function. Use `__eq__` function in order to test equality.
It has a `__Dtext__` marker set to true.

### Death/Burial

```
type burial =
  { type : str
  ; date : date
  }
```

```
type death =
  { death_reason : str
  ; date: date option
  }
```

`death_reason` is either
`"DeadYoung"`, `"DeadDontKnowWhen"`, `"DontKnowIfDead"`, `"OfCourseDead"`,
`"Killed"`, `"Murdered"`, `"Executed"`, `"Disappeared"` or `"Unspecified"`.

### Event

```
type event =
  { date : date
  ; kind : str
  ; name : str
  ; note : str
  ; place : str
  ; spouse : person
  ; src : str
  ; witnesses : witness list
  }
```

A `witness` is a `person` with an extra `kind : str` field.

## Conf

Global `conf` object is available in templates.

```
type conf =
  { access_by_key : bool
  ; allowed_titles : str list
  ; api_host : str
  ; api_port : int
  ; auth_file : str
  (* ; auth_scheme : auth_scheme *)
  ; authorized_wizards_notes : bool
  ; b_arg_for_basename : bool
  ; benv : str assoc
  ; bname : str
  ; border : int
  ; can_send_image : bool
  ; cgi_passwd : str
  ; charset : str
  ; command : str
  ; ctime : float
  ; default_lang : str
  ; default_sosa_ref : person
  ; denied_titles : str list
  ; env : str assoc
  ; friend : bool
  ; from : str
  ; henv : str assoc
  ; hide_names : bool
  ; highlight : str
  ; image_prefix : str
  ; indep_command : str
  ; is_printed_by_template : bool
  ; is_rtl : bool
  ; just_friend_wizard : bool
  ; lang : str
  ; left : str
  ; lexicon : lexicon pat
  ; link_to_referer : str
  ; manitou : bool
  ; multi_parents : bool
  (* ; n_connect : n_connect *)
  ; no_image : bool
  ; no_note : bool
  ; private_years : int
  ; public_if_no_date : bool
  ; public_if_titles : bool
  ; pure_xhtml : bool
  ; request : str list
  ; right : str
  ; senv : str assoc
  ; setup_link : bool
  ; supervisor : bool
  ; time : time
  ; today : dmy
  ; today_wd : int
  ; use_restrict : bool
  ; user : str
  ; username : str
  ; wizard : bool
  ; xhs : str
  }
```

## Miscellaneous

`json_encode` function is available to... encode a value in json format.

`trans` filter will use `lexicon`. If first unnamed parameter is an `int`,
it uses the `nth` function of geneweb's lexicon. Named arguments are used
to fill text in `foo bar:::XXX:YYY` format. Names of argument are not used,
only the order of named arguments matters.

## OPT module

```
val map : ('a -> 'b) func -> [ 'a | Tnull ] -> [ 'a | Tnull ]
```
## DATE module

```
val calendar : str -> date -> date
```
Convert a date into another calendar.
`str` is destination calendar and may be
`"Dgregorian"` `"Djulian"` `"Dfrench"` or `"Dhebrew"`

```
val compare : date -> date -> int
```

```
val death_symbol : str
```

```
val code_french_year -> date -> str
```

```
val eq : date -> date -> bool
```

```
val now : date
```

```
val string_of_age : date -> str
```

```
val string_of_ondate : date -> str
```

```
val sub : date -> date -> date
```
