defmodule EightballAgentWeb.AnswerController do
  use EightballAgentWeb, :controller

  def index(conn, _params) do
    answer = %{name: "Elixir+Phoenix", response: Enum.random(0..19)}
    json conn, answer
  end
end
