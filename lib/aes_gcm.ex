defmodule AES.GCM do
  @moduledoc """
  Provides AES/GCM-256 encoder/decoder
  """

  @aad "AES256GCM"

  @doc """
  `secret_key`
  returns secret key used for encode/decode operations, will get one from env or use stub if missing
  do not forget to store uniq private key in production env
  """

  def secret_key(), do: :application.get_env(:n2o, :secret, "ThisIsClassified")

  @doc """
  Decode tht term

  ## Examples

      iex> n2o:depickle(binary()) -> term().

  """

  def depickle(hex) do
    try do
      cipher = :n2o_secret.unhex(hex)
      <<iv::binary-16, tag::binary-16, bin::binary>> = cipher
      term = :crypto.block_decrypt(:aes_gcm, secret_key(), iv, {@aad, bin, tag})
      :erlang.binary_to_term(term, [:safe])
    rescue
      _ -> ""
    end
  end

  @doc """
  Encode tht term

  ## Examples

      iex> n2o:pickle(term()) -> binary().

  """

  def pickle(term) do
    bin = :erlang.term_to_binary(term)
    iv = :crypto.strong_rand_bytes(16)
    {cipher, tag} = :crypto.block_encrypt(:aes_gcm, secret_key(), iv, {@aad, bin})
    bin = iv <> tag <> cipher
    :n2o_secret.hex(bin)
  end
end
