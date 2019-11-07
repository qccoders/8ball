package qccoders.agent;

import java.util.Random;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class AnswerController {
    private Random r = new Random();

    @RequestMapping("/answer")
    public Answer answer() {
        return new Answer("Java+Spring", r.nextInt(20));
    }

    private class Answer {

        private final int response;
        private final String name;
    
        public Answer(String name, int response) {
            this.name = name;
            this.response = response;
        }
    
        public long getResponse() {
            return response;
        }
    
        public String getName() {
            return name;
        }
    }
}