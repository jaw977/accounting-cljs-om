# accounting-cljs-om

Double entry bookkeeping web app written in ClojureScript and om.

To build the app:  lein cljsbuild auto accounting

## Features

- Allow import / entry of transactions
- Track any type of asset, such as stock shares and their value
- Summary by account and asset type.  
- Register showing all transactions affecting an account and the account's running total.

## Todo

- Add link to pre-built app on this page
- Add validation enforcing balanced transactions
- Save imported / entered transactions and prices to in-browser storage (PouchDB)
- Date range filters
- Report showing monthly or quarterly summary
- Export summary, register, reports to CSV

## LICENSE

The MIT License (MIT)

Copyright (c) 2015 Jason Waag

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
