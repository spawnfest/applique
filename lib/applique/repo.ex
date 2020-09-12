defmodule Applique.Repo do
  use Ecto.Repo,
    otp_app: :applique,
    adapter: Ecto.Adapters.Postgres
end
