  $ herbc ../../../../../test/lookup_negative/000_no_entry.herb
  File '../../../../../test/lookup_negative/000_no_entry.herb', line 0, column 0:
  Error: This module must have an entrypoint
  Define an entry point using `entry { ... }`
  $ herbc ../../../../../test/lookup_negative/001_undefined_toplevel_name.herb
  File '../../../../../test/lookup_negative/001_undefined_toplevel_name.herb', line 5, column 4:
  Error: Undefined variable name 'c'
  $ herbc ../../../../../test/lookup_negative/002_undefined_local_name.herb
  File '../../../../../test/lookup_negative/002_undefined_local_name.herb', line 2, column 12:
  Error: Undefined variable name 'z'
  $ herbc ../../../../../test/lookup_negative/003_undefined_name_2.herb
  File '../../../../../test/lookup_negative/003_undefined_name_2.herb', line 8, column 17:
  Error: Undefined variable name 'id'

