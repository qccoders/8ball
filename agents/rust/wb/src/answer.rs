use rand::Rng;

use Sentiment::*;

#[derive(Clone, Copy)]
pub enum Sentiment {
    Positive,
    Neutral,
    Negative
}

pub fn choose_answer(sentiment: Sentiment) -> u8 {
    match sentiment {
        Negative => random(10),
        Neutral  => random(5) + 10,
        Positive => random(5) + 15
    }
}

fn random(range: u8) -> u8 {
    let mut rng = rand::thread_rng();
    let random_number: u8 = rng.gen();
    random_number % range
}