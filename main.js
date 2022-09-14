

const _view = require("./view/console-view")
const _controller = require("./controller/controller")
const view = _view.from(null)
const controller = _controller.from(view)
controller.start()

