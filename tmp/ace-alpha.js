
let page_num = 1 
let email = '{{ email }}'
let baseSafeRef = '{{ base }}' 
let name = ''
let videos = ['', '', '']
var videoIndex = 1
let job_type = '{{ job_type }}' // '{{ job_type }}'
let questionsLarge = '{{ questions }}' //list

let document = new Document() 

//let question_one = getQuestion(1, job_type)
document.getElementById('question').innerHTML = "Choose a question from below the video player and state what question you are answering in your recording"
//let qList = document.getElementById('qList')
//for (let i=0; i < questionsLarge.length; i++) {
//   var questionElem = document.createElement("P") 
//   questionElem.innerText = questionsLarge[i]
//   qList.append(questionElem)
//}
//function getConstraintObj() {

//return constraintObj
//}

function handleDestroyOutputs() {
    //destroy videos and then maybe implement: delete the button once the response === 200
    console.log('attempting to destroy outputs...')
    videos = document.querySelectorAll(".playbackVideos")
    console.log(videos)
    button = document.querySelectorAll(".inputTag")
    textElem = document.querySelectorAll(".warningMessage")
    var i;
    for (i=0; i < videos.length; i++) {
        videos[i].remove()
        textElem[i].remove()
        button[i].remove()
        //button[i].style = "visibility: hidden;"
    }
}

function getQuestion(page_num, job_type) {
    console.log('attempted getQUestion')
    const extQuestions = {
        'UX UI Design': ['', 'Tell me about a time you overcame an obstacle', 'What are your strengths & weaknesses'],
        'Software Developer': ['Tell me about yourself', 'Tell me about a time you overcame an obstacle', 'What are your strengths & weaknesses'],
        'Data Science': ['Tell me about yourself', 'Tell me about a time you overcame an obstacle', 'What are your strengths & weaknesses'],
        'Civil Engineering': ['Tell me about yourself', 'Tell me about a time you overcame an obstacle', 'What are your strengths & weaknesses'],
        'Biomedical Engineering': ['Tell me about yourself', 'Tell me about a time you overcame an obstacle', 'What are your strengths & weaknesses'],
        'Chemical Engineering': ['Tell me about yourself', 'Tell me about a time you overcame an obstacle', 'What are your strengths & weaknesses'],
        'Sales': ['Tell me about yourself', 'Tell me about a time you overcame an obstacle', 'What are your strengths & weaknesses'],
        'Communications': ['Tell me about yourself', 'Tell me about a time you overcame an obstacle', 'What are your strengths & weaknesses'],
        'Business Analysis': ['Tell me about yourself', 'Tell me about a time you overcame an obstacle', 'What are your strengths & weaknesses'],
        'Accounting': ['Tell me about yourself', 'Tell me about a time you overcame an obstacle', 'What are your strengths & weaknesses'],
        'Human Resources': ['Tell me about yourself', 'Tell me about a time you overcame an obstacle', 'What are your strengths & weaknesses'],
        'Legal': ['Tell me about yourself', 'Tell me about a time you overcame an obstacle', 'What are your strengths & weaknesses'],
        'Educational': ['Tell me about yourself', 'Tell me about a time you overcame an obstacle', 'What are your strengths & weaknesses'],
        'Management': ['Tell me about yourself', 'Tell me about a time you overcame an obstacle', 'What are your strengths & weaknesses'],
        'Other': ['Tell me about yourself', 'Tell me about a time you overcame an obstacle', 'What are your strengths & weaknesses']
    }
    //might change to dictionary of lists

    let q = extQuestions[job_type][page_num-1]
    return q
}

function setPageData(page_num, job_type) {
    //sets title and question
    console.log('attempting to set page data')
    document.getElementById('QuestionTitle').innerHTML = 'Question ' + page_num.toString() + ' /3'
    document.getElementById('question').innerHTML = getQuestion(page_num, job_type)
    document.getElementById('isRecording').innerHTML = "Not recording!"
    document.getElementById('isRecording').style = "color: red;"
    videoIndex = 1;
}



function getXHRobj() {
    const xhr = new XMLHttpRequest();
    url = baseSafeRef + "/recorder/" + email + "/" + job_type + "/"
    xhr.open("POST", url, true) //true allows program to keep running while request is out being processed
    xhr.setRequestHeader("Content-Type", "video/mp4") //octet stream refers to the blob //must be same type as blob instantiation
    xhr.onreadystatechange = function() {
        if (xhr.readyState == XMLHttpRequest.DONE) {
            var status = xhr.status;
            if (status === 0 || (status >= 200 && status < 400)) {
                //request completed successfully 
                if (xhr.readyState === 4) {
                    console.log(xhr.statusCode)
                    console.log(xhr.status)
                    console.log(typeof(xhr.responseText))
                    console.log(xhr.responseText)
                    
                    buttons = document.querySelectorAll(".btn btn-primary inputTag")
                    var j;
                    for (j=0; j < buttons.length; j++) {
                        buttons[j].remove()
                    }
                    
                    //url2 = baseSafeRef + "/recorder/" + e + "/" 
                }
            } else {
                //failure 
                const xhr3 = new XMLHttpRequest();
                xhr3.open("GET", baseSafeRef + "/recorder/failure", true)
            }
        } else {
            console.log(xhr.readyState)
        }
    }
    return xhr 
}

let constraintObj = {
    audio: true,
    video: true
}

navigator.mediaDevices.getUserMedia(constraintObj)
    .then(function (mediaStreamObj) {
        let video = document.getElementById("imgOnlyVid")
        video.srcObject = mediaStreamObj
	
        // OUTPUT VIDEO WITHOUT SOUND TO USER WHILE RECORDING
        video.onloadedmetadata = function (event) {
            video.play()
        }

        //add listeners for saving video 
        let startButt = document.getElementById("startRec")
        let stopButt = document.getElementById("stopRec")
        let mediaRecorder = new MediaRecorder(mediaStreamObj) //, { mimeType: ''}   )
        let chunks = [] //turn into blob 

        startButt.addEventListener('click', (ev) => {

            //here : do change display: 'recording'
            //conditionally based on success of check
            x = document.getElementById('isRecording')
            x.innerHTML = "Recording";
            x.style="color: green;"
            mediaRecorder.start()
            console.log(mediaRecorder.state)

        })
        stopButt.addEventListener('click', (ev) => {

            x=document.getElementById('isRecording')
            x.innerHTML = "Not recording! Video created below :)"
            //x.style = "color: orange"
            mediaRecorder.stop()
            console.log(mediaRecorder.state)
        })
        mediaRecorder.ondataavailable = function (ev) {
            chunks.push(ev.data)
        }
        mediaRecorder.onstop = (ev) => {
            let vidBlob = new Blob(chunks, { 'type': 'video/mp4;' }) //note that 'new' keyword constructs a base <ARG> -> Class<ARG> ; constructs obj from Class
            chunks = []
            let videoURL = URL.createObjectURL(vidBlob)
            console.log(videoURL)
            // will also be used as filename for HTTP form

            console.log(videoURL)
            console.log(vidBlob)

            var playbackVideo = document.createElement("VIDEO") // playbackVideo"
            playbackVideo.id = "playbackVideo"
            playbackVideo.setAttribute("class", "playbackVideos")
            playbackVideo.defaultMuted = false;
            playbackVideo.muted = false;
            playbackVideo.setAttribute('controls', 'controls')
            playbackVideo.style = "height:50%;width:90%;margin-bottom: 10%; border-radius: 20px"
            playbackVideo.src = videoURL

            parentElem = document.getElementById('theMainThing')
            parentElem.appendChild(playbackVideo)

            inpTag = document.createElement("BUTTON")   
            inpTag.style = "margin-left: 3.5%; width: 100%; height: 100%; margin-bottom: 3%;" 
            inpTag.type = "button"
            inpTag.setAttribute('class', 'btn btn-primary inputTag')
            inpTag.id = "userSubmitVideo"
            inpTag.innerHTML = 'Submit Video ' + videoIndex  
            //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            inpTag.addEventListener('click', (ev) => {            
                //immediately change the question and title / anything in the display

                //Still need to implement setting to 'Not Recording' as failsafe

                page_num++;
                if (page_num < 4) {
                    console.log('page # is ' + page_num)
                    setPageData(page_num, job_type)
                } else { 
                    // go to thank you page
                    pageContent = document.getElementById('parElem')
                    //remove children -- does not include navbar
                    pageContent.innerHTML = ''
                    //insert thankyou page 
                    //highlight "please wait" message
                    pageContent.insertAdjacentHTML('afterbegin', '<h2 style="margin-bottom: 10%; margin-top: 10%;">Please wait a couple seconds to make sure your last video uploaded!</h2><h2 style="margin-bottom: 10%; margin-top: 10%;">Thank you for trying out the Ace Alpha! Keep an eye out - an Ace feedback report will be sent to you within 1-2 days!</h2><ul class="list-unstyled"><a href="https://aceinterviewprep.ca/"><li class="media"><img src="/static/images/ace-blue.png" class="mr-3" alt="..." style="width: 100px;"><div class="media-body"><h5 class="mt-0 mb-1">Back to Ace Site</h5></div></li></a><a href="https://www.facebook.com/pathwayshire/"><li class="media my-4"><img src="/static/images/facebook.png" class="mr-3" alt="..." style="width: 100px;"><div class="media-body"><h5 class="mt-0 mb-1">Like us on Facebook</h5></div></li></a><a href="https://www.linkedin.com/company/acetheinterview/"><li class="media"><img src="/static/images/linkedin.png" class="mr-3" alt="..." style="width: 100px;"><div class="media-body"><h5 class="mt-0 mb-1">Follow us on LinkedIn</h5></div></li></a></ul>')
                    //can't do 
                    //window.location = baseSafeRef + '/thankYouSooooooooooooooooooooooooooooooooooMuch/'
                }
                //all below is destroying and sending an xhr 
                console.log('got here')
                const xhr = getXHRobj()
                handleDestroyOutputs()
                xhr.send(vidBlob)
            });
            //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            videoIndex++;
            buttonParent = document.getElementById("buttPara")
            buttonParent.appendChild(inpTag)

            var warningMessage = document.createElement("P")
            warningMessage.innerHTML = "Please don't click away from page before redirect or your data will be lost ... alpha version ... sorry"
            warningMessage.setAttribute('class', 'warningMessage')
            buttonParent.appendChild(warningMessage) 
        };
    })
