defmodule AES.GCM do

  @aad "AES256GCM"

  def secret_key(), do: :application.get_env(:n2o,:secret,"ThisIsClassified")

  def depickle(cipher) do
    <<iv::binary-16, tag::binary-16, bin::binary>> = cipher
    :crypto.block_decrypt(:aes_gcm, secret_key(), iv, {@aad, bin, tag})
  end

  def pickle(plain) do
    iv = :crypto.strong_rand_bytes(16)
    {cipher, tag} = :crypto.block_encrypt(:aes_gcm, secret_key(), iv, {@aad, plain})
    iv <> tag <> cipher
  end

end
