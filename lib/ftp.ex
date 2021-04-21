defmodule N2O.FTP do
  require N2O
  require Kernel

  def root() do
     {:ok, cwd} = :file.get_cwd()
    :filename.join(cwd, :application.get_env(:n2o,:upload,:code.priv_dir(:n2o)))
  end

  def chunk(), do: 256 * 1024

  def fileName(N2O.ftp(filename: fileName)), do: fileName

  def event({:ftpInit, N2O.ftp(id: guid, status: "init", filename: fileName) = ftp, pid}) do
    IO.inspect("DEBUG 1")
    filePath = :filename.join(root(), fileName)
    :filelib.ensure_dir(filePath)
    Supervisor.start_link([], strategy: :one_for_one, name: FTP)

    try do
      :n2o_pi.stop(:ftp, guid)
    catch e -> IO.inspect(e, label: "N2O PI STOP ERROR")
    end
    initFtp = N2O.ftp(ftp, block: chunk(), offset: 0, data: <<>>)
    N2O.pi(
      module: FTP,
      table: :ftp,
      sup: FTP,
      state: {initFtp, []},
      timeout: 180000,
      name: guid
    )
    |> :n2o_pi.start()
    IO.inspect(pid, label: "PID")
    :erlang.send(pid, {:direct, {:ftp_init, initFtp}})

    apply(:nitro,:wire,["ftp_init('#{guid}', 0, #{chunk()}, '#{fileName}')"])
  end

  def event({:ftpSend, N2O.ftp(id: guid, status: "send") = ftp, pid}) do
    IO.inspect(ftp, label: "DEBUG 2")
    N2O.ftp(size: totalSize, offset: offset, block: block, filename: fileName) = ftp2 =
      try do
        :n2o_pi.send(:ftp, guid, {:send, ftp, pid})
      catch _ -> N2O.ftp(ftp, data: <<>>, block: 0)
      end
    IO.inspect(ftp2, label: "DEBUG 3")
    apply(:nitro,:wire,["ftp_send('#{guid}', #{totalSize}, #{offset}, #{block}, '#{fileName}')"])
  end

  def proc(:init, N2O.pi(state: {N2O.ftp() = ftp, _}) = pi) do
    IO.inspect(ftp, label: "N2O PI INIT")
    {:ok, N2O.pi(pi, state: {ftp, ping(100)})}
  end

  def proc({:check}, N2O.pi(state: {x, timer}) = pi) do
    :erlang.cancel_timer(timer)
    new_timer = ping(30000)
    {:noreply, N2O.pi(pi, state: {x, new_timer})}
  end

  def proc({:send, N2O.ftp(status: "send", data: data, block: block) = ftp, web_pid}, N2O.pi(name: guid, state: {N2O.ftp(filename: fileName, size: totalSize, offset: offset), timer}) = pi) when offset + block >= totalSize do
    filePath = :filename.join(root(), fileName)
    finishFtp = N2O.ftp(ftp, data: <<>>, offset: totalSize, filename: fileName, block: 0)
    case :file.write_file(filePath, :erlang.iolist_to_binary(data), [:append, :raw]) do
      :ok ->
        spawn(fn -> :erlang.send(web_pid, {:direct, {:ftp_finish, finishFtp}}) end)
        spawn(fn -> :n2o_pi.stop(:ftp, guid) end)
        {:stop, :normal, finishFtp, N2O.pi(pi, state: {finishFtp, timer})}
      {:error, _} = x -> {:reply, x, pi}
    end
  end

  def proc({:send, N2O.ftp(status: "send", data: data, block: block) = ftp, _web_pid}, N2O.pi(state: {N2O.ftp(filename: fileName, offset: offset), timer}) = pi) do
    IO.inspect("N2O PI SEND")
    filePath = :filename.join(root(), fileName)
    nextFtp = N2O.ftp(ftp, data: <<>>, offset: offset + block, filename: fileName)
    case :file.write_file(filePath, :erlang.iolist_to_binary(data), [:append, :raw]) do
      :ok -> {:reply, nextFtp, N2O.pi(pi, state: {nextFtp, timer})}
      {:error, _} = x -> {:reply, x, pi}
    end
  end

  def proc(_, N2O.pi(state: {_, timer}) = pi), do: {:reply, :ok, N2O.pi(pi, state: {[], timer})}

  def ping(milliseconds), do: :erlang.send_after(milliseconds, self(), {:check})

end
