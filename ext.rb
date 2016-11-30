Lyp.install_extension(__FILE__)

require 'fileutils'
require 'net/telnet'

SERVER_CODE = <<EOF
\\version "2.19.37"
\\require "lys"
#(lys:start-server)
EOF

module Lyp::Lilypond
  LYS_SERVER_PATH = File.join(Lyp::TMP_ROOT, "lys-server.ly")
  
  class << self
    alias_method :orig_parse_lilypond_arg, :parse_lilypond_arg
    def parse_lilypond_arg(arg, argv, argv_clean, options)
      case arg
      when '-s', '--server'
        options[:server] = true
      else
        orig_parse_lilypond_arg(arg, argv, argv_clean, options)
      end
    end

    alias_method :orig_invoke, :invoke
    def invoke(argv, opts = {})
      if opts[:server]
        invoke_with_server(argv, opts)
      else
        orig_invoke(argv, opts)
      end
    end

    def invoke_with_server(argv, opts)
      session = setup_server_session
      cmd = "(lys:compile-file #{FileUtils.pwd.inspect} #{argv.map {|a| a.inspect}.join(" ")})"

      session.cmd(cmd) {|c| puts c.strip}
      session.close
    end

    def setup_server_session
      port = allocate_server_port(current_lilypond)
      
      server = start_server_session(port)
      msg = server.waitfor(/>/)
      puts msg.lines[0].chomp
      server
    end

    def allocate_server_port(lilypond_path)
      1225
    end

    def start_server_session(port)
      Net::Telnet::new("Host" => "localhost", "Port" => port)
    rescue Errno::ECONNREFUSED
      start_server(port)
      Net::Telnet::new("Host" => "localhost", "Port" => port)
    end

    def start_server(port)
      compile([LYS_SERVER_PATH], mode: :spawn, spawn_opts: {
        [:out, :err] => "/tmp/lilypond-server-#{port}.log",
        [:in] => "/dev/null"
      })
      sleep 0.1
      wait_for_server(port)
    end

    def wait_for_server(port)
      loop do
        begin
          session = Net::Telnet::new("Host" => "localhost", "Port" => port)
          session.close
          break
        rescue Errno::ECONNREFUSED
          sleep 0.1
        end
      end
    end
  end
end

unless File.exists?(Lyp::Lilypond::LYS_SERVER_PATH)
  File.open(Lyp::Lilypond::LYS_SERVER_PATH, "w+") {|f| f << SERVER_CODE}
end

