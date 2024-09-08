using UnityEngine;

public class ThrusterController : MonoBehaviour
{
  
    // 3 principles
    // Main goal is that it should look cool
    // Don't frustrate the user
    // Take every short cut you can
    
    
    
    // use a hierarchy: Rotating around the own axis reserves front and crossed to the back
    // side to side can steal a bit of the forward rotation
    
    
    
    
    // every thruster will revert to a default position (use a curve)
    
    
    // first leave out down and backwards as actual movements, polish later
    
    // ring and flapper
    
    // forward movement:
    // the ring needs to be rotated 90 degrees, flapper can be anything (maybe later move down for highspeed)
    
    // side to side:
    // the flappers on one side has to move down, up on the other
    // the rings can not be rotated 90 degrees here. maybe just remove a bit of that
    // e.g. reserve a bit for max side movement.
    // entire drone should rotate though, looks way better. So maybe keep the thrusters subtle
    
    // rotate around the own axis:
    // reserve a front and back on the otherside for each direction.
    // This has priority over all the others
    
    
    
    // up and down:
    // if the drone isn't moving forward, could just happen by adding more thrust or reversing it
    // so the only is when the thrusters are in forward rotation
    // in that case it would be cool if the drone tilts (maybe the tilt is based on the current forward)
    // the thrusters then should all rotate a bit
    // not sure yet how to handle when side to side is happening also
    // could just try it out, make some sliders etc.
    
    
}
