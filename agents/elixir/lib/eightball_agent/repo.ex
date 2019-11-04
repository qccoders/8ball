defmodule EightballAgent.Repo do
  use Ecto.Repo,
    otp_app: :eightball_agent,
    adapter: Ecto.Adapters.Postgres
end
