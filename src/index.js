import './main.css'
import { Main } from './Main.elm'
import registerServiceWorker from './registerServiceWorker'
import { MDCRipple } from '@material/ripple/dist/mdc.ripple'

var app = Main.embed(document.getElementById('root'))

registerServiceWorker()

var jsPlayer, jsPlayerID, jsPlayerVideoID, jsPlayerListerner

app.ports.elmData.subscribe(({ tag, data }) => {
  switch (tag) {
    case 'PageLoaded':
      destroyRipple()
      setRipple()
      break

    case 'PlayerLoaded':
      console.log('PlayerLoaded', data)
      destroyRipple()
      setRipple()
      loadYouTubeVideo(data)
      break

    case 'PlayVideo':
      jsPlayer.playVideo()
      break

    case 'PauseVideo':
      jsPlayer.pauseVideo()
      break

    case 'StopVideo':
      jsPlayer.stopVideo()
      break

    case 'GetPlayerCurrTime':
      app.ports.jsData.send({
        tag: 'JSPlayerCurrTime',
        data: jsPlayer.getCurrentTime()
      })
      break

    case 'SeekTo':
      console.log('SeekTo -> ', data)
      if (jsPlayer) {
        jsPlayer.seekTo(data, true)
      }
      break

    default:
      break
  }
})

const loadYouTubeVideo = ({ playerID, youTubeID }) => {
  var scripts = document.getElementsByTagName('script')
  var ytIframePresent = false
  Array.from(scripts).forEach(script => {
    if (script.src === 'https://www.youtube.com/iframe_api') {
      ytIframePresent = true
      return
    }
  })

  if (!ytIframePresent) {
    var tag = document.createElement('script')
    tag.src = 'https://www.youtube.com/iframe_api'
    var firstScriptTag = document.getElementsByTagName('script')[0]
    firstScriptTag.parentNode.insertBefore(tag, firstScriptTag)
  }

  window.onYouTubeIframeAPIReady = getYouTubeIframeAPIReady(playerID, youTubeID)
  if (jsPlayer) {
    jsPlayer.destroy()
    window.onYouTubeIframeAPIReady()
  }
}

const getYouTubeIframeAPIReady = (jsPlayerID, jsPlayerVideoID) => () => {
  jsPlayer = new YT.Player(jsPlayerID, {
    height: 'auto',
    width: 'auto',
    videoId: jsPlayerVideoID,
    playerVars: {
      autoplay: 0,
      controls: 0,
      fs: 0,
      iv_load_policy: 3,
      modestbranding: 1,
      playsinline: 1,
      rel: 0,
      showinfo: 0
    },
    events: {
      onStateChange: onPlayerStateChange
    }
  })
}

const onPlayerStateChange = e => {
  app.ports.jsData.send({ tag: 'JSPlayerStatus', data: e.data })
}

/* RIPPLE */

var ripple = []
const setRipple = () => {
  setTimeout(() => {
    let surfaces = document.querySelectorAll('.ripple')
    surfaces.forEach(surface => {
      ripple.push(new MDCRipple(surface))
    })
  }, 100)
}

const destroyRipple = () => {
  if (ripple && ripple.length > 0) {
    ripple.forEach(r => {
      r.destroy()
    })
    ripple = []
  }
}
