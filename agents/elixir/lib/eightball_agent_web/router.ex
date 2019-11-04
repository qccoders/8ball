defmodule EightballAgentWeb.Router do
  use EightballAgentWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", EightballAgentWeb do
    pipe_through :browser

    get "/", PageController, :index
  end

  scope "/api/", EightballAgentWeb do
    pipe_through :api

    get "/answer", AnswerController, :index
  end

  # Other scopes may use custom stacks.
  # scope "/api", EightballAgentWeb do
  #   pipe_through :api
  # end
end
