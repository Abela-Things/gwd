open Geneweb
open Def
open Data

let dmy ?(prec = Sure) day month year =
  { year ; month ; day ; delta = 0 ; prec }

let dmy2 day2 month2 year2 = { day2 ; month2 ; year2 ; delta2 = 0 }

let date d = mk_date (Dgreg (d, Dgregorian) )

let conf
    ?(access_by_key = false)
    ?(allowed_titles = lazy [])
    ?(api_host = "")
    ?(api_port = -1)
    ?(auth_file = "")
    ?(auth_scheme = Config.NoAuth)
    ?(authorized_wizards_notes = false)
    ?(b_arg_for_basename = false)
    ?(base_env = [])
    ?(bname = "")
    ?(border = -1)
    ?(can_send_image = false)
    ?(cancel_links = false)
    ?(cgi_passwd = "")
    ?(charset = "")
    ?(command = "")
    ?(ctime = 0.)
    ?(default_lang = "")
    ?(default_sosa_ref = (Adef.iper_of_int (-1), None))
    ?(denied_titles = lazy [])
    ?(env = [])
    ?(friend = false)
    ?(from = "")
    ?(henv = [])
    ?(hide_names = false)
    ?(highlight = "")
    ?(image_prefix = "")
    ?(indep_command = "")
    ?(is_printed_by_template = false)
    ?(is_rtl = false)
    ?(just_friend_wizard = false)
    ?(lang = "")
    ?(left = "")
    ?(lexicon = Hashtbl.create 0)
    ?(manitou = false)
    ?(multi_parents = false)
    ?(n_connect = None)
    ?(no_image = false)
    ?(no_note = false)
    ?(private_years = -1)
    ?(public_if_no_date = false)
    ?(public_if_titles = false)
    ?(pure_xhtml = false)
    ?(request = [])
    ?(right = "")
    ?(senv = [])
    ?(setup_link = false)
    ?(supervisor = false)
    ?(time = (0, 0, 0))
    ?(today = { Def.day=0; month=0; year=0; prec = Def.Sure; delta=0 })
    ?(today_wd = -1)
    ?(use_restrict = false)
    ?(user = "")
    ?(username = "")
    ?(wizard = false)
    ?(xhs = "")
    ()
  =
{ Config.access_by_key; allowed_titles; api_host; api_port; auth_file;
  auth_scheme; authorized_wizards_notes; b_arg_for_basename ; base_env;
  bname; border : int; can_send_image; cancel_links; cgi_passwd;
  charset; command; ctime; default_lang; default_sosa_ref;
  denied_titles; env; friend; from; henv; hide_names; highlight;
  image_prefix; indep_command; is_printed_by_template; is_rtl;
  just_friend_wizard; lang; left; lexicon; manitou; multi_parents;
  n_connect; no_image; no_note; private_years; public_if_no_date;
  public_if_titles; pure_xhtml; request; right; senv; setup_link;
  supervisor; time; today : dmy; today_wd : int; use_restrict; user;
  username; wizard; xhs; }

