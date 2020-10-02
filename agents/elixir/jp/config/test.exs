use Mix.Config

# Configure your database
config :eightball_agent, EightballAgent.Repo,
  username: "postgres",
  password: "postgres",
  database: "eightball_agent_test",
  hostname: "localhost",
  pool: Ecto.Adapters.SQL.Sandbox

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :eightball_agent, EightballAgentWeb.Endpoint,
  http: [port: 4002],
  server: false

# Print only warnings and errors during test
config :logger, level: :warn
