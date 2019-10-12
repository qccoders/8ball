extern crate lambda_runtime as lambda;
extern crate log;
extern crate rand;
extern crate rusoto_comprehend;
extern crate rusoto_core;
extern crate serde;
extern crate simple_logger;

// External
use lambda::{ Context, lambda };
use lambda::error::HandlerError;
use log::{ Level, info, warn };
use rusoto_comprehend::{ Comprehend, ComprehendClient, DetectSentimentResponse, DetectSentimentRequest };
use rusoto_core::Region;
use serde::{ Deserialize, Serialize };

// Internal
mod answer;
use answer::{ Sentiment, choose_answer };

// Standard
use std::error::Error;

#[derive(Deserialize, Serialize)]
struct EightBallRequest {
    message: String,
}

#[derive(Serialize)]
struct EightBallResponse {
    name: String,
    response: u8
}

const LOG_LEVEL:Level = Level::Warn;

fn main() -> Result<(), Box<dyn Error>> {
    simple_logger::init_with_level(LOG_LEVEL)?;
    lambda!(handler);

    Ok(())
}

fn handler(e: EightBallRequest, _c: Context) -> Result<EightBallResponse, HandlerError> {
    info!("Request received: {}", serde_json::to_string(&e)?);

    let client = ComprehendClient::new(Region::UsEast2);

    let language_code = String::from("en");
    let sentiment_request = DetectSentimentRequest { language_code, text: e.message };
    let sentiment_response = client.detect_sentiment(sentiment_request).sync();

    let comprehend_sentiment: Sentiment;
    if let Ok(DetectSentimentResponse { sentiment: Some(top_sentiment), .. }) = sentiment_response {
        comprehend_sentiment = match top_sentiment.as_str() {
            "MIXED" | "NEUTRAL" => Sentiment::Neutral,
            "NEGATIVE" => Sentiment::Negative,
            "POSITIVE" | _ => Sentiment::Positive
        };
    } else {
        warn!("No top sentiment");
        comprehend_sentiment = Sentiment::Positive;
    }

    let response = EightBallResponse {
        name: String::from("pessimist"),
        response: choose_answer(comprehend_sentiment)
    };

    info!("Sending response: {}", serde_json::to_string(&response)?);

    Ok(response)
}
