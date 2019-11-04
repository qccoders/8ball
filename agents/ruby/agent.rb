require 'sinatra'

srand

get '/answer' do
    content_type :json
    { :name => "Ruby+Sinatra", :response => rand(0..19) }.to_json
end