namespace f_.Controllers

open Microsoft.AspNetCore.Mvc
open Microsoft.Extensions.Logging
open f_

[<ApiController>]
[<Route("[controller]")>]
type AnswerController (logger : ILogger<AnswerController>) =
    inherit ControllerBase()

    [<HttpGet>]
    member __.Get() : Answer =
        let rng = System.Random()

        { Name = "F#";
          Response = rng.Next(0,20);
          Children = [|
            { Name = "F♭";
              Response = rng.Next(0,20); 
              Children = [|
                { Name = "F";
                  Response = rng.Next(0,20);
                  Children = null; }
              |]; }
          |]}