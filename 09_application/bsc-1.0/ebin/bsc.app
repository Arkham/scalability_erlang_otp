{application, bsc,
  [{description, "Base Station Controller"},
   {vsn, "1.0"},
   {modules, [bsc_sup, frequency, hlr, frequency_sup, freq_overload, simple_phone_sup]},
   {applications, [kernel, stdlib, sasl]},
   {start_phases, [{init, []}, {admin, []}, {oper, []}]},
   {env, []},
   {mod, {bsc, []}}]}.
