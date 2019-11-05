using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;

namespace c_.Controllers
{
    [ApiController]
    [Route("[controller]")]
    public class AnswerController : ControllerBase
    {
        [HttpGet]
        public IActionResult Get()
        {
            var rng = new Random();

            return Ok(new { 
                Name = "C#",
                Response = rng.Next(0,20),
                Children = new [] {
                    new {
                        Name = "C♭",
                        Response = rng.Next(0,20),
                        Children = new [] {
                            new {
                                Name = "C",
                                Response = rng.Next(0,20)
                            }
                        }
                    }
                }
            });
        }
    }
}
