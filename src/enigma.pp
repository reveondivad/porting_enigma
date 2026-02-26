# Enigma cipher in Puppet (infrastructure as code)
class enigma (
  Array[Integer] $rotor_fwd_1 = [4,10,12,5,11,6,3,16,21,25,13,19,14,22,24,7,23,20,18,15,0,8,1,17,2,9],
  Array[Integer] $rotor_fwd_2 = [0,9,3,10,18,8,17,20,23,1,11,7,22,19,12,2,16,6,25,13,15,24,5,21,14,4],
  Array[Integer] $rotor_fwd_3 = [1,3,5,7,9,11,2,15,17,19,23,21,25,13,24,4,8,22,6,0,10,12,20,18,16,14],
  Array[Integer] $reflector   = [24,17,20,7,16,18,11,3,15,23,13,6,14,10,12,8,4,1,5,25,2,22,21,9,0,19],
  Array[Integer] $notches     = [16, 4, 21],
) {
  # Wehrmacht Enigma I configuration as Puppet resource
  file { '/etc/enigma/rotor1.conf':
    ensure  => file,
    content => inline_template('<%= @rotor_fwd_1.join(",") %>'),
  }
  file { '/etc/enigma/rotor2.conf':
    ensure  => file,
    content => inline_template('<%= @rotor_fwd_2.join(",") %>'),
  }
  file { '/etc/enigma/rotor3.conf':
    ensure  => file,
    content => inline_template('<%= @rotor_fwd_3.join(",") %>'),
  }
  file { '/etc/enigma/reflector.conf':
    ensure  => file,
    content => inline_template('<%= @reflector.join(",") %>'),
  }

  notify { 'enigma_status':
    message => "Enigma I configured: 3 rotors, Reflector B, notches at ${notches}",
  }
}
