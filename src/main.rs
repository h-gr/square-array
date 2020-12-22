use itertools::Itertools;
use rayon::prelude::*;
use std::cmp;
use rand::Rng;
use std::collections::LinkedList;
const N: i64 = 100_000_000;
//const N: i64 = 8;

fn binary_search(slice : &[i64], elemento:i64, inv:bool)->usize
{
    let element = if inv { -elemento } else { elemento };
    let offseta = if inv { -1 as i32} else { 1 };
    let offsetb = if inv { 0 }  else { 1 };

    if slice.len()==0
    {
        return 0;
    }
    else if slice.len()==1
    {
        if element >= slice[0]
        {
            return 1;
        }
        else
        {
            return 0;
        }
    }
    else
    {
        let mut mid = 0;
        let mut start = 0;
        let mut end = slice.len();
        if element>slice[end-1]
        {
            return end;
        }
        else if element < slice[0]
        {
            return 0;
        }
        while (end as i64)-(start as i64) >=0
        {
            mid = (start + end)/2;
            if element == slice[mid]
            {
                if slice[mid] == slice[((mid as i32)+offseta )as usize %slice.len()]{
                    if inv{
                        end = mid-1;
                    }
                    else{
                        start = mid+1;
                    }
                    
                }
                else{
                    return mid+offsetb;
                }
            }
            else if element < slice[mid]
            {
                end = mid-1;
            }
            else
            {
                start = mid + 1
            }
        }
        if inv{
            while slice[mid]>=element{
                if mid ==0 {
                    return 0;
                }
                else{
                    mid-=1;
                }
            }
            return mid+1;
        }
        else{
            while slice[mid]<=element{
                if mid == slice.len()-1 {
                    return slice.len();
                }
                else{
                    mid+=1;
                }
            }
            return mid;
        }
    }
}

fn partition(neg : &[i64], pos : &[i64])->(Vec<(usize, usize)>,Vec<(usize, usize)>,Vec<(usize, usize)>)
{
    let mut ar = Vec::new();
    let mut br = Vec::new();
    let mut cr = Vec::new();
    if pos.len()>0
    {
        let lenf= pos.len() as f64;
        let mut loga = 1.0;
        if pos.len() != 1
        {
            loga = lenf.log2(); //so that the division doesnt go to infinity
        }
        let chunks_size = ((lenf/loga).ceil()) as usize;
        let chunks_number = (lenf/(chunks_size as f64)).ceil() as usize;
        let mut a = Vec::new();
        a.push(0);
        let mut b = Vec::new();
        b.push(0);
        for i in 0..chunks_number{
            let index = cmp::min((i+1)*chunks_size,pos.len());
            b.push(index);
            if neg.len()>0{
                let aj = binary_search(neg, pos[index-1], true);
                a.push(neg.len()-aj);
            }
            else{
                a.push(0);
            }
        }
        for i in 0..a.len()-1{
            br.push((b[i],b[i+1]));
            ar.push((a[i],a[i+1]));
            cr.push((a[i]+b[i],a[i+1]+b[i+1]));
        }
        if ar[ar.len()-1].1<neg.len(){
            br.push((b[b.len()-1],b[b.len()-1]));
            ar.push((ar[ar.len()-1].1,neg.len()));
            cr.push((br[br.len()-1].0+ar[ar.len()-1].0,br[br.len()-1].1+ar[ar.len()-1].1));
        }
    }
    else{
        br.push((0,0));
        if neg.len()>0{
            ar.push((0,neg.len()));
            cr.push((0,neg.len()));
        }
        else{
            ar.push((0,0));
            cr.push((0,0));
        }
    }
    return (ar,br,cr);
}

fn translate(range:(usize,usize),leng:usize)->(usize,usize){
    return (leng-range.1,leng-range.0);
}

fn seq_merge(neg : &[i64], pos : &[i64], res :&mut[i64] )
{
    let mut ok = if neg.len()==0 { false }  else { true };
    let mut i_pos = 0;
    let mut i_neg = 0;
    if ok{
        i_neg = neg.len()-1;
    }
    for k in 0..(neg.len()+pos.len())
    {
        if !ok{
            res[k]=pos[i_pos]*pos[i_pos];
            i_pos+=1;
        }
        else if i_pos >= pos.len(){
            res[k]=neg[i_neg]*neg[i_neg];
            if i_neg == 0{
                ok = false;
            }
            else
            {
                i_neg-=1;
            }
        }
        else if -neg[i_neg]<=pos[i_pos]{
        res[k]=neg[i_neg]*neg[i_neg];
        if i_neg == 0{
            ok = false;
        }
        else
        {
            i_neg-=1;
        }
        }
        else{
        res[k]=pos[i_pos]*pos[i_pos];
        i_pos+=1;
        }
    }
}




fn main() {
    //let v: Vec<i64> = (-N/2..N/2).collect();
    let mut rng = rand::thread_rng();
    let v: Vec<i64> = (0..N).map(|_| {
        rng.gen_range(-N/2, N/2)}).sorted().collect();

    rayon::ThreadPoolBuilder::new().num_threads(2).build_global().unwrap();
    let mut buffer: Vec<i64> = std::iter::repeat_with(Default::default)
        .take(v.len()).into_iter()
        .collect();
    fast_tracer::svg("array_sq.svg", || {
    //let start = std::time::Instant::now();
    let index = binary_search(&v, 0, false);
    let (neg, pos) = v.split_at(index);
    // println!("{:?}", neg);
    // println!("{:?}", pos);
    let(a,b,c) = partition(neg, pos);
    // let  mut list_slides :Vec<&mut[i64]> = Vec::new();
    // let mut slice = buffer.as_mut_slice();
    // let mut go_on = !slice.is_empty();
    // while go_on {
    //     let (mut now, mut later) = {slice}.split_at_mut(25);
    //     list_slides.push(now);
    //     go_on = !later.is_empty();
    //     slice = later;
    // }
    let mut list_slides :Vec<&mut[i64]> = Vec::new();
    let mut slice = buffer.as_mut_slice();
    for i in  0..c.len() {
        let (now, later) = {slice}.split_at_mut(c[i].1-c[i].0);
        list_slides.push(now);
        slice = later;
    }
    a.par_iter().zip(b.par_iter()).zip(list_slides.into_par_iter()).for_each(|((aa,bb), cc)| {
        
        let aat = translate(*aa, neg.len());
        seq_merge(&neg[aat.0..aat.1], &pos[bb.0..bb.1], cc)
    });


    // let time = start.elapsed();
    // println!("par: {:?}", time);
    // println!("{:?}", a);
    // println!("{:?}", b);
    // a.iter().zip(b.iter()).for_each(|(aa,bb)| {
    //     let aat = translate(*aa, neg.len());
    //     println!("{:?}", &neg[aat.0..aat.1]);
    //     println!("{:?}", &pos[bb.0..bb.1]);
    //     println!("-------------");
    // });
    //println!("{:?}", &buffer[buffer.len()-100..]);
    }).expect("failed saving svg file");
    assert!(buffer[buffer.len()-1]!=0);
    assert!(buffer.windows(2).all(|w| w[0] <= w[1]));
}



