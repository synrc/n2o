casper.test.begin("Basic connectivity", 1, function (test)
{
	test.info("Connecting")
	casper
	.start('http://localhost:8000/static/spa/login.htm')
	.then(function ()
	{
		test.assertHttpStatus(200)
		test.info('Loggin')
		this.click('#loginButton')
	}).waitForSelector('#upload', function()
	{
		test.info('Logged In.')
	})
	.run(function ()
	{
		test.done()
	})
})
