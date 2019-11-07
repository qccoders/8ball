require 'sinatra'

srand
set :port, 5002

get '/answer' do
    content_type :json
    { :name => "Ruby+Sinatra", :response => rand(0..19) }.to_json
end
