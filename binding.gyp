{
  'targets': [
    {
      'target_name': 'node_curl',
      'cflags': ['-Wall', '-O1', '-g', '-fno-inline-functions'],
      'cflags_cc': ['-Wall', '-O1', '-g', '-fno-inline-functions'],
      'sources': ['src/node-curl.cc'],
	'link_settings' : {
		'library_dirs' : [
			'libcurl/lib'
		]
	},
      'libraries': ['-llibcurl'],
      'include_dirs+' : [
        'libcurl/include'
      ]
    }
  ]
}
