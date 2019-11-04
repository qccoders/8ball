defmodule EightballAgentWeb.PageController do
  use EightballAgentWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
